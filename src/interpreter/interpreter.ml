open Batteries;;

open Ast;;
open Ast_pp;;

let logger = Logger_utils.make_logger "Interpreter";;

module Environment = Var_hashtbl;;

let pp_env (env : value Environment.t) =
  let inner =
    env
    |> Environment.enum
    |> Enum.map (fun (x,v) -> pp_var x ^ " = " ^ pp_value v)
    |> Enum.fold
      (fun acc -> fun s -> if acc = "" then s else acc ^ ", " ^ s) ""
  in
  "{ " ^ inner ^ " }"
;;

exception Evaluation_failure of string;;

let lookup env x =
  if Environment.mem env x then
    Environment.find env x
  else
    raise (
      Evaluation_failure (
        "cannot find variable `" ^ (pp_var x) ^ "' in environment `" ^ (pp_env env) ^ "'."
      )
    )
;;

(* FIXME: this functionality is duplicated in ast_wellformedness.
   (Needs fixed upstream.) *)
let bound_vars_of_expr (Expr(cls)) =
  cls
  |> List.map (fun (Clause(x, _)) -> x)
  |> Var_set.of_list
;;

let rec var_replace_expr fn (Expr(cls)) =
  Expr(List.map (var_replace_clause fn) cls)

and var_replace_clause fn (Clause(x, b)) =
  Clause(fn x, var_replace_clause_body fn b)

and var_replace_clause_body fn r =
  match r with
  | Value_body(v) -> Value_body(var_replace_value fn v)
  | Var_body(x) -> Var_body(fn x)
  | Appl_body(x1, x2) -> Appl_body(fn x1, fn x2)
  | Conditional_body(x,p,f1,f2) ->
    Conditional_body(fn x, p, var_replace_function_value fn f1,
                     var_replace_function_value fn f2)
  | Projection_body(x,i) -> Projection_body(fn x, i)
  | Deref_body(x) -> Deref_body(fn x)
  | Update_body(x1,x2) -> Update_body(fn x1, fn x2)
  | Binary_operation_body(x1,op,x2) -> Binary_operation_body(fn x1, op, fn x2) 

and var_replace_value fn v =
  match v with
  | Value_record(Record_value(es)) ->
    Value_record(Record_value(Ident_map.map fn es))
  | Value_function(f) -> Value_function(var_replace_function_value fn f)
  | Value_ref(Ref_value(x)) -> Value_ref(Ref_value(fn x))
  | Value_int n -> Value_int n

and var_replace_function_value fn (Function_value(x, e)) =
  Function_value(fn x, var_replace_expr fn e)

let freshening_stack_from_var x =
  let Var(appl_i, appl_fso) = x in
  (* The freshening stack of a call site at top level is always
     present. *)
  let Freshening_stack idents = Option.get appl_fso in
  Freshening_stack (appl_i :: idents)
;;

let repl_fn_for clauses freshening_stack extra_bound =
  let bound_variables =
    (* FIXME: this functionality is duplicated above in bound_vars_of_expr; this
       needs to be fixed upstream. *)
    clauses
    |> List.map (fun (Clause(x,_)) -> x)
    |> Var_set.of_list
    |> Var_set.union extra_bound 
  in
  let repl_fn (Var(i, _) as x) =
    if Var_set.mem x bound_variables
    then Var(i, Some freshening_stack)
    else x
  in
  repl_fn
;;

let fresh_wire (Function_value(param_x, Expr(body))) arg_x call_site_x =
  (* Build the variable freshening function. *)
  let freshening_stack = freshening_stack_from_var call_site_x in
  let repl_fn =
    repl_fn_for body freshening_stack @@ Var_set.singleton param_x in
  (* Create the freshened, wired body. *)
  let freshened_body = List.map (var_replace_clause repl_fn) body in
  let head_clause = Clause(repl_fn param_x, Var_body(arg_x)) in
  let Clause(last_var,_) = List.last freshened_body in
  let tail_clause = Clause(call_site_x, Var_body(last_var)) in
  [head_clause] @ freshened_body @ [tail_clause]
;;

let rec matches env x p =
  let v = lookup env x in
  match v with
  | Value_record(Record_value(els)) ->
    begin
      match p with
      | Record_pattern(els') ->
        els'
        |> Ident_map.enum
        |> Enum.for_all
          (fun (i,p') ->
             try
               matches env (Ident_map.find i els) p'
             with
             | Not_found -> false
          )
    end
  | Value_function(Function_value(_)) -> false
  | Value_ref(Ref_value(_)) -> false
  | Value_int _ -> false
;;

let rec evaluate env lastvar cls =
  logger `debug (
    pp_env env ^ "\n" ^
    (Option.default "?" (Option.map pp_var lastvar)) ^ "\n" ^
    (cls
     |> List.map pp_clause
     |> List.fold_left (fun acc -> fun s -> acc ^ s ^ "; ") "") ^ "\n\n");
  flush stdout;
  match cls with
  | [] ->
    begin
      match lastvar with
      | Some(x) -> (x, env)
      | None ->
        (* TODO: different exception? *)
        raise (Failure "evaluation of empty expression!")
    end
  | (Clause(x, b)):: t ->
    begin
      match b with
      | Value_body(v) ->
        Environment.add env x v;
        evaluate env (Some x) t
      | Var_body(x') ->
        let v = lookup env x' in
        Environment.add env x v;
        evaluate env (Some x) t
      | Appl_body(x', x'') ->
        begin
          match lookup env x' with
          | Value_function(f) ->
            evaluate env (Some x) @@ fresh_wire f x'' x @ t
          | r -> raise (Evaluation_failure
                          ("cannot apply " ^ pp_var x' ^
                           " as it contains non-function " ^ pp_value r))
        end
      | Conditional_body(x',p,f1,f2) ->
        let f_target = if matches env x' p then f1 else f2 in
        evaluate env (Some x) @@ fresh_wire f_target x' x @ t
      | Projection_body(x', i) ->
        begin
          match lookup env x' with
          | Value_record(Record_value(els)) as r ->
            begin
              try
                let x'' = Ident_map.find i els in
                let v = lookup env x'' in
                Environment.add env x v;
                evaluate env (Some x) t
              with
              | Not_found ->
                raise @@ Evaluation_failure("cannot project " ^ pp_ident i ^
                                  " from " ^ pp_value r ^ ": not present")
            end
          | v ->
            raise @@ Evaluation_failure("cannot project " ^ pp_ident i ^
                                " from non-record value " ^ pp_value v)
        end
      | Deref_body(x') ->
        let v = lookup env x' in
        begin
          match v with
          | Value_ref(Ref_value(x'')) ->
            let v' = lookup env x'' in
            Environment.add env x v';
            evaluate env (Some x) t
          | _ -> raise (Evaluation_failure
                          ("cannot dereference " ^ pp_var x' ^
                           " as it contains non-reference " ^ pp_value v))
        end
      | Update_body(x', x'') ->
        let v = lookup env x' in
        let v' = lookup env x'' in
        begin
          match v with
          | Value_ref(Ref_value(x'')) ->
            Environment.replace env x'' v';
            evaluate env (Some x)
              ((Clause(x,
                  Value_body(Value_record(Record_value(Ident_map.empty)))))::t)
          | _ -> raise (Evaluation_failure
                          ("cannot update " ^ pp_var x' ^
                           " as it contains non-reference " ^ pp_value v))
        end
      | Binary_operation_body(x1,op,x2) ->
        let v1 = lookup env x1 in
        let v2 = lookup env x2 in
        let result =
          begin
            let bool_of b =
              if b
              then Value_record(Record_value(Ident_map.empty))
              else Value_int(0)
            in
            match v1,op,v2 with
            | (Value_int(n1),Binary_operator_int_plus,Value_int(n2)) ->
              Value_int(n1+n2)
            | (Value_int(n1),Binary_operator_int_minus,Value_int(n2)) ->
              Value_int(n1-n2)
            | (Value_int(n1),Binary_operator_int_less_than,Value_int(n2)) ->
              bool_of (n1 < n2)
            | ( Value_int(n1)
              , Binary_operator_int_less_than_or_equal_to
              , Value_int(n2)
              ) ->
              bool_of (n1 <= n2)
            | (Value_int(n1),Binary_operator_int_equal_to,Value_int(n2)) ->
              bool_of (n1 = n2)
            | v1,op,v2 ->
              raise @@ Evaluation_failure(
                Printf.sprintf "Cannot complete binary operation: (%s) %s (%s)"
                  (pp_value v1) (pp_binary_operator op) (pp_value v2))
          end
        in
        Environment.add env x result;
        evaluate env (Some x) t
    end
;;

let eval (Expr(cls)) =
  let env = Environment.create(20) in
  let repl_fn = repl_fn_for cls (Freshening_stack []) Var_set.empty in
  let cls' = List.map (var_replace_clause repl_fn) cls in
  evaluate env None cls'
;;