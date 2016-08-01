open Batteries;;
open Egg_translator;;

module Ident_map = Core_ast.Ident_map;;

let swan_to_egg_var v =
  match v with
  | Swan_ast.Swan_var(u, x) ->
    Egg_ast.Egg_var(u, x)
;;

let rec swan_to_egg_pattern p =
  match p with
  | Swan_ast.Fun_pattern u ->
    Egg_ast.Fun_pattern u
  | Swan_ast.Ref_pattern u ->
    Egg_ast.Ref_pattern u
  | Swan_ast.Int_pattern u ->
    Egg_ast.Int_pattern u
  | Swan_ast.Bool_pattern(u,b) ->
    Egg_ast.Bool_pattern(u,b)
  | Swan_ast.String_pattern u ->
    Egg_ast.String_pattern u
  | Swan_ast.Any_pattern u ->
    Egg_ast.Any_pattern u
  | Swan_ast.Record_pattern(u,elements) ->
    let enum = Ident_map.enum elements in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map swan_to_egg_pattern values in
    let values = Enum.combine (keys, rec_result) in
    let egg_elts = Ident_map.of_enum values in
    Egg_ast.Record_pattern(u, egg_elts)
  | Swan_ast.List_pattern(u,elements) ->
    Egg_ast.List_pattern(u, List.map swan_to_egg_pattern elements)
  | Swan_ast.Cons_pattern(u,p_element,p_list) ->
    Egg_ast.Cons_pattern(u,swan_to_egg_pattern p_element,swan_to_egg_pattern p_list)
  | Swan_ast.Var_pattern(u1,Swan_ast.Swan_var(u2, identifier)) ->
    Egg_ast.Var_pattern(u1,Egg_ast.Egg_var(u2, identifier))
  | Swan_ast.Variant_pattern(u1,Swan_ast.Variant(variant_name),elements) ->
    Egg_ast.Variant_pattern(u1,Egg_ast.Variant(variant_name),
                                elements |> List.map swan_to_egg_pattern)
;;

let rec swan_to_egg_expr e =
  match e with
  | Swan_ast.Record_expr(uid, elements) ->
    let enum = Ident_map.enum elements in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map swan_to_egg_expr values in
    let values = Enum.combine (keys, rec_result) in
    let egg_elts = Ident_map.of_enum values in
    Egg_ast.Record_expr(uid, egg_elts)
  | Swan_ast.Sequencing_expr(u,e_left,e_right) ->
    Egg_ast.Sequencing_expr(u,swan_to_egg_expr e_left,swan_to_egg_expr e_right)
  | Swan_ast.List_expr(u,elements) ->
    Egg_ast.List_expr(u, List.map swan_to_egg_expr elements)
  | Swan_ast.Cons_expr(u,e_element,e_list) ->
    Egg_ast.Cons_expr(u,swan_to_egg_expr e_element,swan_to_egg_expr e_list)
  | Swan_ast.Function_expr(uid, fv) ->
    Egg_ast.Function_expr(uid, swan_to_egg_function_value fv)
  | Swan_ast.Int_expr(uid, n) ->
    Egg_ast.Int_expr(uid, n)
  | Swan_ast.Bool_expr(uid, b) ->
    Egg_ast.Bool_expr(uid, b)
  | Swan_ast.String_expr(uid, s) ->
    Egg_ast.String_expr(uid, s)
  | Swan_ast.Ref_expr(uid, e) ->
    Egg_ast.Ref_expr(uid, swan_to_egg_expr e)
  | Swan_ast.Var_expr(uid, x) ->
    Egg_ast.Var_expr(uid, swan_to_egg_var x)
  | Swan_ast.Appl_expr(uid, e1, es2) ->
    Egg_ast.Appl_expr(uid, swan_to_egg_expr e1, es2 |> List.map swan_to_egg_expr)
  | Swan_ast.Conditional_expr(uid, e, p, fv1, fv2) ->
    Egg_ast.Conditional_expr(
      uid
    , swan_to_egg_expr e
    , swan_to_egg_pattern p
    , swan_to_egg_function_value fv1
    , swan_to_egg_function_value fv2
    )
  | Swan_ast.If_expr(uid, e, e1, e2) ->
    Egg_ast.If_expr(
      uid
    , swan_to_egg_expr e
    , swan_to_egg_expr e1
    , swan_to_egg_expr e2
    )
  | Swan_ast.Deref_expr(uid, e) ->
    Egg_ast.Deref_expr(uid, swan_to_egg_expr e)
  | Swan_ast.Update_expr(uid, e1, e2) ->
    Egg_ast.Update_expr(
      uid
    , swan_to_egg_expr e1
    , swan_to_egg_expr e2
    )
  | Swan_ast.Binary_operation_expr(uid, e1, op, e2) ->
    Egg_ast.Binary_operation_expr(
      uid
    , swan_to_egg_expr e1
    , op
    , swan_to_egg_expr e2
    )
  | Swan_ast.Unary_operation_expr(uid, op, e) ->
    Egg_ast.Unary_operation_expr(uid, op, swan_to_egg_expr e)
  | Swan_ast.Indexing_expr(uid, e1, e2) ->
    Egg_ast.Indexing_expr(
      uid
    , swan_to_egg_expr e1
    , swan_to_egg_expr e2
    )
  | Swan_ast.Let_expr(uid, x, e1, e2) ->
    Egg_ast.Let_expr(
      uid
    , swan_to_egg_var x
    , swan_to_egg_expr e1
    , swan_to_egg_expr e2
    )
  | Swan_ast.Let_function_expr(uid, x, xs, e1, e2) ->
    Egg_ast.Let_function_expr(
      uid
    , swan_to_egg_var x
    , xs |> List.map swan_to_egg_var
    , swan_to_egg_expr e1
    , swan_to_egg_expr e2
    )
  | Swan_ast.Projection_expr(uid, e, i) ->
    Egg_ast.Projection_expr(uid, swan_to_egg_expr e, i)
  | Swan_ast.Match_expr(uid, e, mps) ->
    let swan_mps = List.enum mps in
    let enum = Enum.map swan_to_egg_match_pair swan_mps in
    let egg_mps = List.of_enum enum in
    Egg_ast.Match_expr(uid, swan_to_egg_expr e, egg_mps)
  | Swan_ast.Variant_expr(u1,Swan_ast.Variant(variant_name),elements) ->
    Egg_ast.Variant_expr(u1,Egg_ast.Variant(variant_name),
                                elements |> List.map swan_to_egg_expr)

  | Swan_ast.Fail_expr(u1,e) -> Egg_ast.Fail_expr(u1, swan_to_egg_expr e)

and swan_to_egg_function_value
    (Swan_ast.Function(u,vs,e')) =
  let body = swan_to_egg_expr e' in
  Egg_ast.Function(u, vs |> List.map swan_to_egg_var, body)

and swan_to_egg_match_pair
    (Swan_ast.Match_pair(u, p, e)) =
  Egg_ast.Match_pair(
    u
  , swan_to_egg_pattern p
  , swan_to_egg_expr e
  )

let egg_to_nested_var (Egg_ast.Egg_var(u,x)) =
  Nested_ast.Nested_var(u, x)

let rec egg_to_nested_pattern p =
  match p with
  | Egg_ast.Fun_pattern u ->
    Nested_ast.Fun_pattern u
  | Egg_ast.Ref_pattern u ->
    Nested_ast.Ref_pattern u
  | Egg_ast.Int_pattern u ->
    Nested_ast.Int_pattern u
  | Egg_ast.Bool_pattern(u,b) ->
    Nested_ast.Bool_pattern(u,b)
  | Egg_ast.String_pattern u ->
    Nested_ast.String_pattern u
  | Egg_ast.Any_pattern u ->
    Nested_ast.Any_pattern u
  | Egg_ast.Record_pattern (u,elements) ->
    let enum = Ident_map.enum elements in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map egg_to_nested_pattern values in
    let values = Enum.combine (keys, rec_result) in
    let nested_elts = Ident_map.of_enum values in
    Nested_ast.Record_pattern(u, nested_elts)
  | Egg_ast.List_pattern(_,_)
  | Egg_ast.Cons_pattern(_,_,_)
  | Egg_ast.Variant_pattern(_,_,_)
  | Egg_ast.Var_pattern(_,_) ->
    raise @@ Utils.Invariant_failure "An egg pattern that was not fully translated has been passed into egg_to_nested"

let rec egg_to_nested_function_value
    (Egg_ast.Function(u,vs,e')) =
  match vs with
  | [v] ->
    let body = egg_to_nested_expr e' in
    Nested_ast.Function(u, egg_to_nested_var v, body)
  | _ ->
    raise @@ Utils.Invariant_failure "An egg function value that was not fully translated has been passed into egg_to_nested"

and egg_to_nested_expr e =
  match e with
  | Egg_ast.Function_expr(u,f) ->
    Nested_ast.Function_expr(
      u
    , egg_to_nested_function_value f
    )
  | Egg_ast.Int_expr(u,n) ->
    Nested_ast.Int_expr(u,n)
  | Egg_ast.Bool_expr(u,b) ->
    Nested_ast.Bool_expr(u,b)
  | Egg_ast.String_expr(u,s) ->
    Nested_ast.String_expr(u,s)
  | Egg_ast.Ref_expr(u,e') ->
    let e_nested = egg_to_nested_expr e' in
    Nested_ast.Ref_expr(u, e_nested)
  | Egg_ast.Var_expr(u,v) ->
    Nested_ast.Var_expr(u, egg_to_nested_var v)
  | Egg_ast.Appl_expr(u,e1,es2) ->
    begin
      match es2 with
      | [e2] ->
        let e1_trans = egg_to_nested_expr e1 in
        let e2_trans = egg_to_nested_expr e2 in
        Nested_ast.Appl_expr(u, e1_trans, e2_trans)
      | _ ->
        raise @@ Utils.Invariant_failure "An egg function application that was not fully translated has been passed into egg_to_nested"
    end
  | Egg_ast.Conditional_expr(u,e',p,f1,f2) ->
    let e_trans = egg_to_nested_expr e' in
    let p_trans = egg_to_nested_pattern p in
    let f1_trans = egg_to_nested_function_value f1 in
    let f2_trans = egg_to_nested_function_value f2 in
    Nested_ast.Conditional_expr(
      u,
      e_trans,
      p_trans,
      f1_trans,
      f2_trans)
  | Egg_ast.Deref_expr(u,e') ->
    let e_trans = egg_to_nested_expr e' in
    Nested_ast.Deref_expr(u, e_trans)
  | Egg_ast.Update_expr(u,e1,e2) ->
    let e1_trans = egg_to_nested_expr e1 in
    let e2_trans = egg_to_nested_expr e2 in
    Nested_ast.Update_expr(u, e1_trans, e2_trans)
  | Egg_ast.Binary_operation_expr(u,e1,op,e2) ->
    let e1_trans = egg_to_nested_expr e1 in
    let e2_trans = egg_to_nested_expr e2 in
    Nested_ast.Binary_operation_expr(u, e1_trans, op, e2_trans)
  | Egg_ast.Unary_operation_expr(u,op,e) ->
    let e_trans = egg_to_nested_expr e in
    Nested_ast.Unary_operation_expr(u, op, e_trans)
  | Egg_ast.Indexing_expr(u,e1,e2) ->
    let e1_trans = egg_to_nested_expr e1 in
    let e2_trans = egg_to_nested_expr e2 in
    Nested_ast.Indexing_expr(u, e1_trans, e2_trans)
  | Egg_ast.Let_expr(u,v,e1,e2) ->
    let e1_trans = egg_to_nested_expr e1 in
    let e2_trans = egg_to_nested_expr e2 in
    Nested_ast.Let_expr(
      u,
      egg_to_nested_var v,
      e1_trans,
      e2_trans)
  | Egg_ast.Projection_expr(u,e',i) ->
    let e_trans = egg_to_nested_expr e' in
    Nested_ast.Projection_expr(u, e_trans, i)
  | Egg_ast.Record_expr(u,elements) ->
    let enum = Ident_map.enum elements in
    let (keys, values) = Enum.uncombine enum in
    let rec_result = Enum.map egg_to_nested_expr values in
    let values = Enum.combine (keys, rec_result) in
    let nested_elts = Ident_map.of_enum values in
    Nested_ast.Record_expr(u, nested_elts)
  | Egg_ast.Sequencing_expr _
  | Egg_ast.Match_expr _
  | Egg_ast.List_expr _
  | Egg_ast.Cons_expr _
  | Egg_ast.Variant_expr _
  | Egg_ast.Let_function_expr _
  | Egg_ast.Fail_expr _
  | Egg_ast.If_expr _ ->
    raise @@ Utils.Invariant_failure "An egg expression that was not fully translated has been passed into egg_to_nested"
;;

let swan_to_nested_translation e =
  let egg_expr = swan_to_egg_expr e in
  let (translator, _) =
    translation_close
      (expression_translator_compose_many expression_translators)
      (pattern_translator_compose_many pattern_translators)
  in
  let (egg_trans, map) = translator egg_expr in
  ((egg_to_nested_expr egg_trans), map)
;;
