(** A module containing utility functions for analyses. *)

open Batteries;;

open Core_ast;;
open Ddpa_graph;;
open Utils;;

(** Obtain the set of all variables appearing within an expression. *)
let find_all_vars e =
  let rec find_all_vars' (Expr cls) =
    cls
    |> List.enum
    |> Enum.map
      (fun (Clause(x,r)) ->
         (* FIXME: extract x from below and concatenate it to reduce
            redundancy *)

         match r with
         | Value_body(v) ->
           begin
             match v with
             | Value_function(f) ->
               Enum.append (Enum.singleton x) @@ find_all_vars_in_fn f
             | _ ->
               Enum.singleton x
           end
         | Var_body(x') -> List.enum [x;x']
         | Appl_body(x',x'') -> List.enum [x;x';x'']
         | Conditional_body(x,_,f1,f2) ->
           Enum.concat @@ List.enum @@
           [ Enum.singleton x
           ; find_all_vars_in_fn f1
           ; find_all_vars_in_fn f2
           ]
         | Projection_body(x',_) -> List.enum [x;x']
         | Deref_body(x') -> List.enum [x;x']
         | Update_body(x',x'') -> List.enum [x;x';x'']
         | Binary_operation_body(x1,_,x2) -> List.enum [x;x1;x2]
         | Unary_operation_body(_,x1) -> List.enum [x;x1]
         | Indexing_body(x1,x2) -> List.enum [x;x1;x2]
      )
    |> Enum.concat
  and find_all_vars_in_fn (Function_value(x,e)) =
    Enum.append (Enum.singleton x) @@ find_all_vars' e
  in
  uniq_enum Var.compare @@ find_all_vars' e
;;

(** Obtain the set of all record projection labels appearing within an
    expression.  (Any record label which is never projected is never necessary
    during lookup. *)
let rec find_all_projection_labels (Expr cls) =
  cls
  |> List.enum
  |> Enum.filter_map
    (fun (Clause(_,r)) ->
       match r with
       | Value_body(v) ->
         begin
           match v with
           | Value_function(f) -> Some (find_all_projection_labels_in_fn f)
           | _ -> None
         end
       | Var_body _ -> None
       | Appl_body _ -> None
       | Conditional_body(_,_,f1,f2) ->
         let e1 = find_all_projection_labels_in_fn f1 in
         let e2 = find_all_projection_labels_in_fn f2 in
         Some (Enum.append e1 e2)
       | Projection_body(_,l) -> Some (Enum.singleton l)
       | Deref_body _ -> None
       | Update_body _ -> None
       | Binary_operation_body _ -> None
       | Unary_operation_body _ -> None
       | Indexing_body _ -> None
    )
  |> Enum.concat

and find_all_projection_labels_in_fn (Function_value(_,e)) =
  find_all_projection_labels e
;;

(** Retrieve the set of "context clauses" appearing anywhere within an
    expression.  These are clauses which may be pushed onto the context stack
    during analysis. *)
let rec extract_context_clauses (Expr cls) =
  let immediate =
    cls
    |> List.enum
    |> Enum.filter
      (function
        | Clause(_,Appl_body(_,_)) -> true
        | Clause(_,Conditional_body(_,_,_,_)) -> true
        | _ -> false)
  in
  let from_function (Function_value(_,e)) = extract_context_clauses e in
  let inner =
    cls
    |> List.enum
    |> Enum.filter_map
      (function
        | Clause(_,Value_body(Value_function(f))) -> Some(from_function f)
        | Clause(_,Conditional_body(_,_,f1,f2)) ->
          Some(Enum.append (from_function f1) (from_function f2))
        | _ -> None)
    |> Enum.concat
  in
  Enum.append immediate inner
;;

(** Iterate recursively over all clauses in an expression. *)
let rec iterate_clauses (Expr(cls)) =
  let top_level = List.enum cls in
  let nested = Enum.delay
      (fun () -> Enum.concat @@
        Enum.map (fun e -> Enum.delay (fun () -> iterate_clauses e)) @@
        Enum.concat @@ List.enum @@ List.map _exprs_of_clause cls)
  in
  Enum.append top_level nested

and _exprs_of_clause (Clause(_,b)) =
  match b with
  | Conditional_body(_,_,Function_value(_,e1),Function_value(_,e2)) ->
    Enum.append (Enum.singleton e1) (Enum.singleton e2)
  | Value_body(v) ->
    _exprs_of_value v
  | Var_body _ | Appl_body _ | Projection_body _ | Deref_body _
  | Update_body _ | Binary_operation_body _ | Unary_operation_body _
  | Indexing_body _ -> Enum.empty ()

and _exprs_of_value v =
  match v with
  | Value_function(Function_value(_,e)) -> Enum.singleton e
  | Value_record _ | Value_ref _ | Value_int _ | Value_bool _
  | Value_string _ -> Enum.empty ()
;;

(** Iterate recursively over all clauses in an expression. *)
let rec iterate_abstract_clauses (Abs_expr(acls)) =
  let top_level = List.enum acls in
  let nested = Enum.delay
      (fun () -> Enum.concat @@
        Enum.map (fun e -> Enum.delay (fun () -> iterate_abstract_clauses e)) @@
        Enum.concat @@ List.enum @@ List.map _abs_exprs_of_clause acls)
  in
  Enum.append top_level nested

and _abs_exprs_of_clause (Abs_clause(_,b)) =
  match b with
  | Abs_conditional_body(_,_,Abs_function_value(_,e1),Abs_function_value(_,e2))
    -> Enum.append (Enum.singleton e1) (Enum.singleton e2)
  | Abs_value_body(v) ->
    _abs_exprs_of_value v
  | Abs_var_body _ | Abs_appl_body _ | Abs_projection_body _ | Abs_deref_body _
  | Abs_update_body _ | Abs_binary_operation_body _ | Abs_unary_operation_body _
  | Abs_indexing_body _ -> Enum.empty ()

and _abs_exprs_of_value v =
  match v with
  | Abs_value_function(Abs_function_value(_,e)) -> Enum.singleton e
  | Abs_value_record _ | Abs_value_ref _ | Abs_value_int | Abs_value_bool _
  | Abs_value_string -> Enum.empty ()
;;
