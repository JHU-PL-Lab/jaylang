open Batteries;;

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "a__" ^ (string_of_int index) in
  Ast.Var(Ast.Ident(name),None)
;;

let rec pattern_of_nested_pattern p =
  match p with
  | Nested_ast.Record_pattern idents ->
    Ast.Record_pattern idents
;;

let rec function_value_of_nested_function_value
    (Nested_ast.Function(x',e')) =
  let (body,_) = clauses_and_var_of_nested_expr e' in
  Ast.Function_value(x',Ast.Expr body)

and clauses_and_var_of_nested_expr e =
  let x = fresh_var() in
  let (clauses,final_body) =
    match e with
    | Nested_ast.Record_expr idents ->
      ([], Ast.Value_body(
          Ast.Value_record(
            Ast.Record_value idents)))
    | Nested_ast.Function_expr(f) ->
      ([], Ast.Value_body(
          Ast.Value_function(
            function_value_of_nested_function_value f)))
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
    | Nested_ast.Let_expr(x',e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @
        [ Ast.Clause(x', Ast.Var_body(x1)) ] @
        cls2
      , Ast.Var_body(x2)
      )
  in
  (clauses @ [Ast.Clause(x,final_body)],x)

let a_translate_nested_expr e =
  let (cls,_) = clauses_and_var_of_nested_expr e in
  Ast.Expr cls
;;