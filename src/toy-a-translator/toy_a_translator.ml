open Batteries;;

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "a__" ^ (string_of_int index) in
  Toy_ast.Var(Toy_ast.Ident(name),None)
;;

let rec pattern_of_nested_pattern p =
  match p with
    | Toy_nested_ast.Record_pattern idents ->
        Toy_ast.Record_pattern idents
;;

let rec function_value_of_nested_function_value
    (Toy_nested_ast.Function(x',e')) =
  let (body,_) = clauses_and_var_of_nested_expr e' in
  Toy_ast.Function_value(x',Toy_ast.Expr body)

and clauses_and_var_of_nested_expr e =
  let x = fresh_var() in
  let (clauses,final_body) =
    match e with
      | Toy_nested_ast.Record_expr idents ->
          ([], Toy_ast.Value_body(
                  Toy_ast.Value_record(
                    Toy_ast.Record_value idents)))
      | Toy_nested_ast.Function_expr(f) ->
          ([], Toy_ast.Value_body(
                  Toy_ast.Value_function(
                    function_value_of_nested_function_value f)))
      | Toy_nested_ast.Var_expr(x') ->
          ([], Toy_ast.Var_body(x'))
      | Toy_nested_ast.Appl_expr(e1,e2) ->
          let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
          let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
          (cls1 @ cls2, Toy_ast.Appl_body(x1,x2))
      | Toy_nested_ast.Conditional_expr(e',p,f1,f2) ->
          let (cls0,x') = clauses_and_var_of_nested_expr e' in
          ( cls0
          , Toy_ast.Conditional_body(
              x', pattern_of_nested_pattern p,
              function_value_of_nested_function_value f1,
              function_value_of_nested_function_value f2))
      | Toy_nested_ast.Let_expr(x',e1,e2) ->
          let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
          let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
          ( cls1 @
            [ Toy_ast.Clause(x', Toy_ast.Var_body(x1)) ] @
            cls2
          , Toy_ast.Var_body(x2)
          )
  in
  (clauses @ [Toy_ast.Clause(x,final_body)],x)

let a_translate_toy_nested_expr e =
  let (cls,_) = clauses_and_var_of_nested_expr e in
  Toy_ast.Expr cls
;;
