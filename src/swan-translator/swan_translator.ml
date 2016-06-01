(*open Batteries;;*)

module Ident_map = Ast.Ident_map;;

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

let translate_swan_expr_to_nested e =
  nested_expr_of_swan_expr e
;;
