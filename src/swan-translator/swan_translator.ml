(*open Batteries;;*)
open Uid;;

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
  | Swan_ast.Record_pattern (_,elements) ->
    Nested_ast.Record_pattern (next_uid (),Ident_map.map nested_pattern_of_swan_pattern elements)
  | Swan_ast.Fun_pattern _ -> Nested_ast.Fun_pattern (next_uid ())
  | Swan_ast.Ref_pattern _ -> Nested_ast.Ref_pattern (next_uid ())
  | Swan_ast.Int_pattern _ -> Nested_ast.Int_pattern (next_uid ())
  | Swan_ast.Bool_pattern(_,b) -> Nested_ast.Bool_pattern(next_uid (),b)
  | Swan_ast.String_pattern _ -> Nested_ast.String_pattern(next_uid ())

let rec nested_function_value_of_swan_function_value
    (Swan_ast.Function(_,x',e')) =
  let body = nested_expr_of_swan_expr e' in
  (Nested_ast.Function(next_uid(),x',body))

and nested_expr_of_swan_expr e =
  match e with
  | Swan_ast.Record_expr(_,t) ->
    (*let t' =
      t
      |> Ident_map.enum
      |> Enum.map (fun (x,y) -> (x, nested_expr_of_swan_expr y))
      |> Ident_map.of_enum
      in Nested_ast.Record_expr(t') *)
    Nested_ast.Record_expr(next_uid (),Ident_map.map nested_expr_of_swan_expr t)
  | Swan_ast.Function_expr(_,f) ->
    Nested_ast.Function_expr(next_uid (),nested_function_value_of_swan_function_value f)
  | Swan_ast.Int_expr(_,n) -> Nested_ast.Int_expr(next_uid (),n)
  | Swan_ast.Bool_expr(_,b) -> Nested_ast.Bool_expr(next_uid (),b)
  | Swan_ast.String_expr(_,s) -> Nested_ast.String_expr(next_uid (),s)
  | Swan_ast.Ref_expr(_,e') ->
    Nested_ast.Ref_expr(next_uid (),(nested_expr_of_swan_expr e'))
  | Swan_ast.Var_expr(_,x) -> Nested_ast.Var_expr(next_uid (),x)
  | Swan_ast.Appl_expr(_,e1,e2) ->
    Nested_ast.Appl_expr(next_uid (),(nested_expr_of_swan_expr e1),
                         (nested_expr_of_swan_expr e2))
  | Swan_ast.Conditional_expr(_,e',p,f1,f2) ->
    Nested_ast.Conditional_expr(next_uid (),
      (nested_expr_of_swan_expr e'),
      (nested_pattern_of_swan_pattern p),
      (nested_function_value_of_swan_function_value f1),
      (nested_function_value_of_swan_function_value f2))
  | Swan_ast.If_expr(_,e',e1,e2) ->
    Nested_ast.Conditional_expr(next_uid (),
      nested_expr_of_swan_expr e',
      Nested_ast.Bool_pattern(next_uid (),true),
      nested_function_value_of_swan_function_value (Swan_ast.Function(next_uid (), fresh_var (), e1)),
      nested_function_value_of_swan_function_value (Swan_ast.Function(next_uid (), fresh_var (), e2))
    )
  | Swan_ast.Deref_expr(_,e') ->
    Nested_ast.Deref_expr(next_uid (),nested_expr_of_swan_expr e')
  | Swan_ast.Update_expr(_,e1,e2) ->
    Nested_ast.Update_expr(next_uid (),
      nested_expr_of_swan_expr e1,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Binary_operation_expr(_,e1,op,e2) ->
    Nested_ast.Binary_operation_expr(next_uid (),
      nested_expr_of_swan_expr e1, op,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Unary_operation_expr(_,op,e1) ->
    Nested_ast.Unary_operation_expr(next_uid (),op,
                                    nested_expr_of_swan_expr e1)
  | Swan_ast.Indexing_expr(_,e1,e2) ->
    Nested_ast.Indexing_expr(next_uid (),
      nested_expr_of_swan_expr e1,
      nested_expr_of_swan_expr e2)
  | Swan_ast.Let_expr(_,x,e1,e2) ->
    Nested_ast.Let_expr(next_uid (),x,
                        nested_expr_of_swan_expr e1,
                        nested_expr_of_swan_expr e2)
  | Swan_ast.Projection_expr(_,e',i) ->
    Nested_ast.Projection_expr(next_uid (),
      nested_expr_of_swan_expr e',i)
  | Swan_ast.Match_expr(_,e, ms) ->
    let x = fresh_var () in
    let rec desugar_matches ms e =
      match ms with
      | Swan_ast.Match_pair(_,p,e')::ms' -> Nested_ast.Conditional_expr(next_uid (),
         nested_expr_of_swan_expr e,
         nested_pattern_of_swan_pattern p,
         (Nested_ast.Function(next_uid (),fresh_var (), nested_expr_of_swan_expr e')),
         (Nested_ast.Function(next_uid (),fresh_var (), desugar_matches ms' e)))
      | _ -> Nested_ast.Appl_expr(next_uid (),(Nested_ast.Record_expr(next_uid (),Ident_map.empty)), (Nested_ast.Record_expr(next_uid (),Ident_map.empty)))
(* raise (Translation_failure_exception(No_match_clauses_in_match_expr(e))) *)
    in let e' = desugar_matches ms e
    in Nested_ast.Let_expr(next_uid (),x, nested_expr_of_swan_expr e, e')

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
