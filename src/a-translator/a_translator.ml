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
  | Nested_ast.Record_pattern(_,elements) ->
    Ast.Record_pattern (Ident_map.map pattern_of_nested_pattern elements)
  | Nested_ast.Fun_pattern(_) -> Ast.Fun_pattern
  | Nested_ast.Ref_pattern(_) -> Ast.Ref_pattern
  | Nested_ast.Int_pattern(_) -> Ast.Int_pattern
  | Nested_ast.Bool_pattern(_,b) -> Ast.Bool_pattern(b)
  | Nested_ast.String_pattern(_) -> Ast.String_pattern
;;

let rec function_value_of_nested_function_value
    (Nested_ast.Function(_,x',e')) =
  let (body,_) = clauses_and_var_of_nested_expr e' in
  Ast.Function_value(x',Ast.Expr body)

and clauses_and_var_of_nested_expr e =
  let x = fresh_var() in
  let (clauses,final_body) =
    match e with
    | Nested_ast.Record_expr(_,elements) ->
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
    | Nested_ast.Function_expr(_,f) ->
      ([], Ast.Value_body(
          Ast.Value_function(
            function_value_of_nested_function_value f)))
    | Nested_ast.Int_expr(_,n) ->
      ([], Ast.Value_body(Ast.Value_int(n)))
    | Nested_ast.Bool_expr(_,b) ->
      ([], Ast.Value_body(Ast.Value_bool(b)))
    | Nested_ast.String_expr(_,s) ->
      ([], Ast.Value_body(Ast.Value_string(s)))
    | Nested_ast.Ref_expr(_,e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      (cls0, Ast.Value_body(Ast.Value_ref(Ast.Ref_value(x'))))
    | Nested_ast.Var_expr(_,x') ->
      ([], Ast.Var_body(x'))
    | Nested_ast.Appl_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      (cls1 @ cls2, Ast.Appl_body(x1,x2))
    | Nested_ast.Conditional_expr(_,e',p,f1,f2) ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Ast.Conditional_body(
          x', pattern_of_nested_pattern p,
          function_value_of_nested_function_value f1,
          function_value_of_nested_function_value f2))
    | Nested_ast.Deref_expr(_,e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Ast.Deref_body(x'))
    | Nested_ast.Update_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Ast.Update_body(x1,x2)
      )
    | Nested_ast.Binary_operation_expr(_,e1,op,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Ast.Binary_operation_body(x1,op,x2)
      )
    | Nested_ast.Unary_operation_expr(_,op,e1) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      (cls1, Ast.Unary_operation_body(op,x1))
    | Nested_ast.Indexing_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Ast.Indexing_body(x1,x2)
      )
    | Nested_ast.Let_expr(_,x',e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @
        [ Ast.Clause(x', Ast.Var_body(x1)) ] @
        cls2
      , Ast.Var_body(x2)
      )
    | Nested_ast.Projection_expr(_,e',i) ->
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
