open Batteries
open Odefa_ast
open Odefa_ddpa
open Ast
open Ddpa_abstract_ast

let heading_var_of_clause = function
  | Unannotated_clause(Abs_clause(Abs_var x, _)) -> Some x
  | Nonbinding_enter_clause(_, Abs_clause(Abs_var x, Abs_conditional_body(_, _, _))) -> Some x
  | Binding_enter_clause(_, _, Abs_clause(Abs_var x, Abs_appl_body(Abs_var _, _))) -> Some x
  | _ -> None

let heading_var_of_clause_exn clause = 
  match heading_var_of_clause clause with
  | Some x -> x
  | None -> failwith "must have a var"

let has_condition_clause clauses =
  List.exists (function 
      (* main condition exp *)
      | Unannotated_clause(Abs_clause(Abs_var _, Abs_conditional_body _)) -> true
      | _ -> false
    ) clauses

let has_application_clause clauses =
  List.exists (function 
      (* callsite exp *)
      | Unannotated_clause(Abs_clause(Abs_var _, Abs_appl_body _)) -> true
      | _ -> false
    ) clauses

let all_fun_start_clauses clauses =
  (* FunTop exp *)
  List.for_all (function
      | Binding_enter_clause(Abs_var _, Abs_var _, _) -> true
      | _ -> false
    ) clauses


(* This is acturally an analysis-to-symbolic-execution adapting function,
   with which the symbolic execution can decouple with the analysis.
   It makes the analysis configurable to the symbolic execution.
*)
(* 
exising
  clause_map:       id -> clause
  heavily used

  clause_pred_map:  id -> clause
  never used

  func_para_map:    para:id -> fun
  used in FunEnter rule

  fun_ret_map:      ret:id -> fun
  used in FunReturn rule

  first_var:        id
  used in several rules

  hybrid_table:     id -> value
 *)

let rec enum_all_functions_in_expr expr : function_value Enum.t =
  let Expr(clauses) = expr in
  Enum.concat @@ Enum.map enum_all_functions_in_clause @@ List.enum clauses
and enum_all_functions_in_clause clause : function_value Enum.t =
  let Clause(_,body) = clause in
  enum_all_functions_in_body body
and enum_all_functions_in_body body : function_value Enum.t =
  match body with
  | Value_body v ->
    enum_all_functions_in_value v
  | Var_body _
  | Input_body
  | Appl_body _
  | Binary_operation_body (_, _, _) ->
    Enum.empty ()
  | Conditional_body (_, e1, e2) ->
    Enum.append
      (enum_all_functions_in_expr e1) (enum_all_functions_in_expr e2)
  | Match_body (_, _)
  | Projection_body (_, _) ->
    Enum.empty ()
and enum_all_functions_in_value value : function_value Enum.t =
  match value with
  | Value_record _ -> Enum.empty ()
  | Value_function(Function_value(_,e) as f) ->
    Enum.append (Enum.singleton f) @@ enum_all_functions_in_expr e
  | Value_int _
  | Value_bool _ -> Enum.empty ()

(* is this the same as Ast_tools.flatten 
   No. expr_flatten returns expr list
   while flatten return clause list.
*)
let rec expr_flatten ((Expr clauses) as expr) : expr list =
  expr ::
  (clauses
   |>
   List.map
     (fun (Clause(_,b)) ->
        match b with
        | Value_body (Value_function(Function_value(_,e))) -> expr_flatten e
        | Value_body _
        | Var_body _
        | Input_body
        | Appl_body (_, _)
        | Match_body (_, _)
        | Projection_body (_, _)
        | Binary_operation_body (_, _, _) -> []
        | Conditional_body (_, e1, e2) ->
          e1 :: e2 :: expr_flatten e1 @ expr_flatten e2
     )
   |> List.concat
  )

let clause_predecessor_mapping e =
  e 
  |>expr_flatten
  |> List.enum
  |> Enum.map
    (fun (Expr clauses) ->
       let c1 = List.enum clauses in
       let c2 = List.enum clauses in
       Enum.drop 1 c1;
       Enum.combine (c1,c2)
       |> Enum.map
         (fun (Clause(Var(x,_),_),clause) -> (x,clause))
    )
  |> Enum.concat
  |> Ident_map.of_enum

let clause_mapping e =
  e
  |> Ast_tools.flatten
  |> List.enum
  |> Enum.fold
    (fun map (Clause(Var(x,_),_) as c) ->
       Ident_map.add x c map
    )
    Ident_map.empty

let first_var e =
  e
  |> (fun (Expr cls) -> cls)
  |> List.first
  |> (fun (Clause(Var(x,_),_)) -> x)

let make_function_mapping e = 
  enum_all_functions_in_expr e
  |> Enum.fold
    (fun (pm, rm, idm) (Function_value(Var(p, _), Expr(clss)) as f) ->
       let pm' = Ident_map.add p f pm in
       let retvar =
         clss
         |> List.last
         |> (fun (Clause(Var(r,_),_)) -> r)
       in
       let rm' = Ident_map.add retvar f rm
       in
       let idm' = Ident_map.add retvar p idm
       in
       (pm', rm', idm')
    )
    (Ident_map.empty, Ident_map.empty, Ident_map.empty)

let make_ret_to_fun_def_mapping e =
  let map = ref Ident_map.empty 
  in
  let rec loop (Expr clauses) =
    match clauses with
    | [] ->
      ()
    | ((Clause(Var (def_x, _), Value_body(Value_function(Function_value(_, function_body)))))) :: rest_clauses ->
      let Var(ret_id, _) = Ast_tools.retv function_body in
      map := Ident_map.add ret_id def_x !map;
      loop function_body;
      loop (Expr rest_clauses);
      ()
    | ((Clause(_, Conditional_body(_, match_body, antimatch_body)))) :: rest_clauses ->
      loop match_body;
      loop antimatch_body;
      loop (Expr rest_clauses);
      ()
    | clause :: rest_clauses ->
      loop (Expr rest_clauses);
      ()
  in
  loop e;
  !map

let make_para_to_fun_def_mapping e =
  let map = ref Ident_map.empty 
  in
  let rec loop (Expr clauses) =
    match clauses with
    | [] ->
      ()
    | ((Clause(Var (def_x, _), Value_body(Value_function(Function_value(Var (para, _), function_body)))))) :: rest_clauses ->
      map := Ident_map.add para def_x !map;
      loop function_body;
      loop (Expr rest_clauses);
      ()
    | ((Clause(_, Conditional_body(_, match_body, antimatch_body)))) :: rest_clauses ->
      loop match_body;
      loop antimatch_body;
      loop (Expr rest_clauses);
      ()
    | clause :: rest_clauses ->
      loop (Expr rest_clauses);
      ()
  in
  loop e;
  !map

let app_id2_of_clause = function
  | Clause (_, Appl_body (_, Var (id2, _))) -> id2
  | _ -> failwith "not app clause"