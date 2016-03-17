open Batteries;;

module Ident_map = Ast.Ident_map;;

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "a__" ^ (string_of_int index) in
  Ast.Var(Ast.Ident(name),None)
;;

let rec pattern_of_nested_pattern p =
  match p with
  | Nested_ast.Record_pattern elements ->
    Ast.Record_pattern (Ident_map.map pattern_of_nested_pattern elements)
;;

let rec function_value_of_nested_function_value
    (Nested_ast.Function(x',e')) =
  let (body,_) = clauses_and_var_of_nested_expr e' in
  Ast.Function_value(x',Ast.Expr body)

and clauses_and_var_of_nested_expr e =
  let x = fresh_var() in
  let (clauses,final_body) =
    match e with
    | Nested_ast.Record_expr elements ->
      let elements' =
        Ident_map.map clauses_and_var_of_nested_expr elements
      in
      let record_body = Ident_map.map snd elements' in
      let extra_clauses =
        elements'
        |> Ident_map.enum
        |> Enum.map (snd %> fst)
        |> List.of_enum
        |> List.concat
      in
      (extra_clauses,
       Ast.Value_body(Ast.Value_record(Ast.Record_value record_body)))
    | Nested_ast.Function_expr(f) ->
      ([], Ast.Value_body(
          Ast.Value_function(
            function_value_of_nested_function_value f)))
    | Nested_ast.Int_expr(n) ->
      ([], Ast.Value_body(Ast.Value_int(n)))
    | Nested_ast.Ref_expr(e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      (cls0, Ast.Value_body(Ast.Value_ref(Ast.Ref_value(x'))))
    | Nested_ast.Var_expr(x') ->
      ([], Ast.Var_body(x'))
    | Nested_ast.Appl_expr(e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      (cls1 @ cls2, Ast.Appl_body(x1,x2))
    | Nested_ast.Conditional_expr(e',p,f1,f2) ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Ast.Conditional_body(
          x', pattern_of_nested_pattern p,
          function_value_of_nested_function_value f1,
          function_value_of_nested_function_value f2))
    | Nested_ast.Deref_expr(e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Ast.Deref_body(x'))
    | Nested_ast.Update_expr(e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Ast.Update_body(x1,x2)
      )
    | Nested_ast.Binary_operation_expr(e1,op,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Ast.Binary_operation_body(x1,op,x2)
      )
    | Nested_ast.Let_expr(x',e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @
        [ Ast.Clause(x', Ast.Var_body(x1)) ] @
        cls2
      , Ast.Var_body(x2)
      )
    | Nested_ast.Projection_expr(e',i) ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Ast.Projection_body(x', i)
      )
  in
  (clauses @ [Ast.Clause(x,final_body)],x)

let a_translate_nested_expr e =
  let (cls,_) = clauses_and_var_of_nested_expr e in
  Ast.Expr cls
;;