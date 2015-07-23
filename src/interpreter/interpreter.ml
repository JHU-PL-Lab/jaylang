open Batteries;;

open Ast;;
open Ast_pretty;;
open Ast_uid;;

let logger = Logger_utils.make_logger "Interpreter";;

module Environment = Var_hashtbl;;

let pretty_env (env : value Environment.t) =
  let inner =
    env
    |> Environment.enum
    |> Enum.map (fun (x,v) -> pretty_var x ^ " = " ^ pretty_value v)
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
          "cannot find variable `" ^ (pretty_var x) ^ "' in environment `" ^ (pretty_env env) ^ "'."
        )
      )
;;

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

and var_replace_value fn v =
  match v with
  | Value_record(_) -> v
  | Value_function(f) -> Value_function(var_replace_function_value fn f)

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
    clauses
    |> List.map (fun (Clause(x, _)) -> x)
    |> Var_set.of_list
    |> Var_set.union extra_bound 
  in
  let repl_fn (Var(i, fso) as x) =
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
  let Clause(last_var, _) = List.last freshened_body in
  let tail_clause = Clause(call_site_x, Var_body(last_var)) in
  [head_clause] @ freshened_body @ [tail_clause]
;;

let rec evaluate env lastvar cls =
  logger `debug (
      pretty_env env ^ "\n" ^
      (Option.default "?" (Option.map pretty_var lastvar)) ^ "\n" ^
      (cls
       |> List.map pretty_clause
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
              | Value_record(_) as r -> raise (Evaluation_failure
                  ("cannot apply " ^ pretty_var x' ^
                    " as it contains non-function " ^ pretty_value r))
              | Value_function(f) ->
                  evaluate env (Some x) @@ fresh_wire f x'' x @ t
          end
      | Conditional_body(x',p,f1,f2) ->
          let successful_match =
                match lookup env x' with
                  | Value_record(Record_value(is)) ->
                      begin
                        match p with
                          | Record_pattern(is') -> Ident_set.subset is' is
                      end
                  | Value_function(Function_value(_)) -> false
          in
          let f_target = if successful_match then f1 else f2 in
          evaluate env (Some x) @@ fresh_wire f_target x' x @ t            
;;

let eval (Expr(cls)) =
  let env = Environment.create(20) in
  let repl_fn = repl_fn_for cls (Freshening_stack []) Var_set.empty in
  let cls' = List.map (var_replace_clause repl_fn) cls in
  evaluate env None cls'
;;