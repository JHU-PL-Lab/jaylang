(*open Batteries;;*)

module Ident_map = Ast.Ident_map;;

type translation_failure =
  | No_match_clauses_in_match_expr of Swan_ast.expr
  [@@ deriving eq, ord, show]

exception Translation_failure_exception of translation_failure

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "s__" ^ (string_of_int index) in
  Ast.Var(Ast.Ident(name),None)
;;

let rec nested_pattern_of_swan_pattern p =
  match p with
  | Swan_ast.Record_pattern elements ->
    Nested_ast.Record_pattern (Ident_map.map nested_pattern_of_swan_pattern elements)
  | Swan_ast.Fun_pattern -> Nested_ast.Fun_pattern
  | Swan_ast.Ref_pattern -> Nested_ast.Ref_pattern
  | Swan_ast.Int_pattern -> Nested_ast.Int_pattern
  | Swan_ast.Bool_pattern(b) -> Nested_ast.Bool_pattern(b)
  | Swan_ast.String_pattern -> Nested_ast.String_pattern

let rec nested_function_value_of_swan_function_value
    (Swan_ast.Function(x',e')) =
  let body = nested_expr_of_swan_expr e' in
  (Nested_ast.Function(x',body))

and nested_expr_of_swan_expr e =
  match e with
  | Swan_ast.Record_expr(t) ->
    (*let t' =
      t
      |> Ident_map.enum
      |> Enum.map (fun (x,y) -> (x, nested_expr_of_swan_expr y))
      |> Ident_map.of_enum
      in Nested_ast.Record_expr(t') *)
    Nested_ast.Record_expr(Ident_map.map nested_expr_of_swan_expr t)
  | Swan_ast.Function_expr(f) ->
    Nested_ast.Function_expr(nested_function_value_of_swan_function_value f)
  | Swan_ast.Int_expr(n) -> Nested_ast.Int_expr(n)
  | Swan_ast.Bool_expr(b) -> Nested_ast.Bool_expr(b)
  | Swan_ast.String_expr(s) -> Nested_ast.String_expr(s)
  | Swan_ast.Ref_expr(e') ->
    Nested_ast.Ref_expr((nested_expr_of_swan_expr e'))
  | Swan_ast.Var_expr(x) -> Nested_ast.Var_expr(x)
  | Swan_ast.Appl_expr(e1,e2) ->
    Nested_ast.Appl_expr((nested_expr_of_swan_expr e1),
                         (nested_expr_of_swan_expr e2))
  | Swan_ast.Conditional_expr(e',p,f1,f2) ->
    Nested_ast.Conditional_expr(
      (nested_expr_of_swan_expr e'),
      (nested_pattern_of_swan_pattern p),
      (nested_function_value_of_swan_function_value f1),
      (nested_function_value_of_swan_function_value f2))
  | Swan_ast.If_expr(e',e1,e2) ->
    Nested_ast.Conditional_expr(
      nested_expr_of_swan_expr e',
      Nested_ast.Bool_pattern(true),
      nested_function_value_of_swan_function_value (Swan_ast.Function(fresh_var (), e1)),
      nested_function_value_of_swan_function_value (Swan_ast.Function(fresh_var (), e2))
    )
  | Swan_ast.Deref_expr(e') ->
    Nested_ast.Deref_expr(nested_expr_of_swan_expr e')
  | Swan_ast.Update_expr(e1,e2) ->
    Nested_ast.Update_expr(
      nested_expr_of_swan_expr e1,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Binary_operation_expr(e1,op,e2) ->
    Nested_ast.Binary_operation_expr(
      nested_expr_of_swan_expr e1, op,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Unary_operation_expr(op,e1) ->
    Nested_ast.Unary_operation_expr(op,
                                    nested_expr_of_swan_expr e1)
  | Swan_ast.Indexing_expr(e1,e2) ->
    Nested_ast.Indexing_expr(
      nested_expr_of_swan_expr e1,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Let_expr(x,e1,e2) ->
    Nested_ast.Let_expr(x,
                        nested_expr_of_swan_expr e1,
                        nested_expr_of_swan_expr e2)
  | Swan_ast.Projection_expr(e',i) ->
    Nested_ast.Projection_expr(
      nested_expr_of_swan_expr e',i)
  | Swan_ast.Match_expr(e, ms) ->
    let x = fresh_var () in
    let rec desugar_matches ms e =
      match ms with
      | Swan_ast.Match_pair(p,e')::ms' -> Nested_ast.Conditional_expr(
         nested_expr_of_swan_expr e,
         nested_pattern_of_swan_pattern p,
         (Nested_ast.Function(fresh_var (), nested_expr_of_swan_expr e')),
         (Nested_ast.Function(fresh_var (), desugar_matches ms' e)))
      | _ -> Nested_ast.Appl_expr((Nested_ast.Record_expr(Ident_map.empty)), (Nested_ast.Record_expr(Ident_map.empty)))
(* raise (Translation_failure_exception(No_match_clauses_in_match_expr(e))) *)
    in let e' = desugar_matches ms e
    in Nested_ast.Let_expr(x, nested_expr_of_swan_expr e, e')

    (*Generate a fresh variable which we will refer to as x and has the value e
     * Get the first match pair from ms, see if x matches its pattern
     * If it does, return that match pair's expression
     * Else, recursively do the same with the rest of ms
     * If we get to an empty ms and still no match has been reached, fail
    *)

    (* Pseudocode:*
     * let fresh_var() = x in
       * let x = e' in
        * match ms with
          * | a:as ->
              neated_expr_of Swan_ast.Conditional_expr(e',a.p, nested_expr_of a.e, nested_expr_of Match_expr(x, as))
          * | [] -> fail
     *)

let translate_swan_expr_to_nested e =
  nested_expr_of_swan_expr e
;;
