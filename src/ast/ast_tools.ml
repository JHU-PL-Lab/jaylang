open Batteries;;

open Ast;;

let rec find_all_variables_in_expr (Expr cls) =
  cls
  |> List.enum
  |> Enum.map find_all_variables_in_clause
  |> Enum.concat
   
and find_all_variables_in_clause (Clause(x,b)) =
  Enum.append (Enum.singleton x) @@ find_all_variables_in_clause_body b
  
and find_all_variables_in_clause_body b =
  match b with
  | Value_body v -> find_all_variables_in_value v
  | Var_body x -> Enum.singleton x
  | Appl_body(x1,x2) -> List.enum [x1;x2]
  | Conditional_body(x,p,f1,f2) ->
    Enum.concat @@ List.enum
    [ Enum.singleton x
    ; find_all_variables_in_pattern p
    ; find_all_variables_in_function_value f1
    ; find_all_variables_in_function_value f2
    ]
  | Projection_body(x,_) -> Enum.singleton x
  | Deref_body x -> Enum.singleton x
  | Update_body(x1,x2) -> List.enum [x1;x2]
  | Binary_operation_body(x1,_,x2) -> List.enum [x1;x2]
  | Unary_operation_body(_,x1) -> Enum.singleton x1

and find_all_variables_in_value v =
  match v with
  | Value_record(Record_value m) ->
    Ident_map.values m
  | Value_function f -> find_all_variables_in_function_value f
  | Value_ref(Ref_value x) -> Enum.singleton x
  | Value_int _ -> Enum.empty ()
  | Value_bool _ -> Enum.empty ()

and find_all_variables_in_function_value (Function_value(x,e)) =
  Enum.append (Enum.singleton x) @@ find_all_variables_in_expr e

and find_all_variables_in_pattern _ = Enum.empty ()

;;

let rec find_all_clauses_in_expr (Expr cls) =
  cls
  |> List.enum
  |> Enum.map find_all_clauses_in_clause
  |> Enum.concat
   
and find_all_clauses_in_clause (Clause(_,b) as cl) =
  Enum.append (Enum.singleton cl) @@ find_all_clauses_in_clause_body b
  
and find_all_clauses_in_clause_body b =
  match b with
  | Value_body v -> find_all_clauses_in_value v
  | Var_body _ -> Enum.empty ()
  | Appl_body _ -> Enum.empty ()
  | Conditional_body(_,_,f1,f2) ->
    Enum.concat @@ List.enum
    [ find_all_clauses_in_function_value f1
    ; find_all_clauses_in_function_value f2
    ]
  | Projection_body _ -> Enum.empty ()
  | Deref_body _ -> Enum.empty ()
  | Update_body _ -> Enum.empty ()
  | Binary_operation_body _ -> Enum.empty()
  | Unary_operation_body _ -> Enum.empty()

and find_all_clauses_in_value v =
  match v with
  | Value_record _ -> Enum.empty ()
  | Value_function f -> find_all_clauses_in_function_value f
  | Value_ref _ -> Enum.empty ()
  | Value_int _ -> Enum.empty ()
  | Value_bool _ -> Enum.empty ()

and find_all_clauses_in_function_value (Function_value(_,e)) =
  find_all_clauses_in_expr e

;;
