(** A module containing utility functions for analyses. *)

open Batteries;;

open Ast;;
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
  uniq_enum Var_order.compare @@ find_all_vars' e
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
