(**
  This module contains a number of routines to validate the well-formedness of
  an AST.
*)

open Batteries;;
open Printf;;

open Toy_ast;;
open Toy_ast_pretty;;
open Toy_ast_uid;;
open Toy_string_utils;;
open Toy_utils;;

type illformedness =
  | Filter_cycle of var list
  | Open_filter_variable of var
  | Duplicate_variable_binding of var
  | Open_expression_variable of var
;;

exception Illformedness_found of illformedness list;;

let pretty_illformedness ill =
  match ill with
    | Filter_cycle(xs) ->
        sprintf "Pattern variable cycle detected: %s"
          (pretty_list pretty_var xs)
    | Open_filter_variable(x) ->
        sprintf "Free variable detected in pattern: %s" (pretty_var x)
    | Duplicate_variable_binding(x) ->
        sprintf "Variable %s bound twice" (pretty_var x)
    | Open_expression_variable(x) ->
        sprintf "Variable %s is free in this expression" (pretty_var x)
;;

let merge_illformedness xs =
  let ills = 
    xs
      |> List.enum
      |> Enum.map
          (fun f ->
            try
              f (); []
            with
              | Illformedness_found(ills) -> ills)
      |> Enum.map List.enum
      |> Enum.concat
      |> List.of_enum
  in
  if not @@ List.is_empty ills
    then raise (Illformedness_found(ills))
;;

(**
  Determines the variables bound by an expression.
*)
let vars_bound_by_expr (Expr(cls)) =
  Var_set.of_list @@ List.map (fun (Clause(x,_)) -> x) cls
;;

(**
  Determines the variables free in an expression.
*)
let rec vars_free_in_expr (Expr(cls_initial)) =
  let rec walk cls =
    match cls with
      | [] -> Var_set.empty
      | (Clause(x,b))::t ->
          let free_t = walk t in
          let free_h =
            match b with
              | Var_body x'  -> Var_set.singleton x'
              | Value_body(v) ->
                  begin
                    match v with
                      | Value_function(f) -> walk_fn f
                      | Value_record(Record_value(_)) -> Var_set.empty
                  end
              | Appl_body(x1',x2') -> Var_set.of_list [x1';x2']
              | Conditional_body(x',p,f1,f2) ->
                  List.fold_left Var_set.union Var_set.empty
                    [ Var_set.singleton x'
                    ; walk_fn f1
                    ; walk_fn f2
                    ]
          in
          Var_set.remove x @@ Var_set.union free_h free_t
  and walk_fn (Function_value(x',e)) =
    Var_set.remove x' @@ vars_free_in_expr e
  in
  walk cls_initial
;;

(**
  Determines if an expression is well-formed.
*)
let check_wellformed_expr e_initial : unit =
  let rec check_closed e =
    let free = vars_free_in_expr e in
    if Var_set.cardinal free > 0
      then raise (Illformedness_found(
        free |> Var_set.enum |> Enum.map (fun x -> Open_expression_variable(x))
             |> List.of_enum))
  in
  let rec check_unique_bindings (Expr(cls_initial)) =
    let merge_count_maps m1 m2 =
      let merge_fn k n1o n2o =
        match (n1o,n2o) with
          | (Some n1, None) -> Some n1
          | (None, Some n2) -> Some n2
          | (Some n1, Some n2) -> Some (n1 + n2)
          | (None, None) -> None
      in
      Var_map.merge merge_fn m1 m2
    in
    let rec count_clause_bindings cls =
      cls
        |> List.enum
        |> Enum.map
            (fun (Clause(x,b)) ->
              let extras =
                match b with
                  | Value_body(Value_function(Function_value(x',(Expr(cls'))))) ->
                      let pat_map = Var_map.singleton x' 1 in
                      let body_map = count_clause_bindings cls' in
                      merge_count_maps pat_map body_map
                  | _ -> Var_map.empty
              in
              merge_count_maps extras @@ Var_map.singleton x 1
            )
        |> Enum.fold merge_count_maps Var_map.empty
    in
    let violations =
          count_clause_bindings cls_initial
            |> Var_map.enum
            |> Enum.filter_map
                  (fun (x,n) -> if n > 1 then Some x else None)
            |> List.of_enum
    in
    if not @@ List.is_empty violations
      then raise (Illformedness_found(
              List.map (fun x -> Duplicate_variable_binding(x)) violations))
  in
  (* These must be done sequentially to satisfy invariants of the validation
     steps. *)
  check_closed e_initial;
  check_unique_bindings e_initial
;;
