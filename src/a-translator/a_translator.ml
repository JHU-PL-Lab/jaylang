open Batteries;;

module Ident_map = Core_ast.Ident_map;;

let fresh_var_counter = ref 0;;

let fresh_var () =
  let index = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  let name = "a__" ^ (string_of_int index) in
  Core_ast.Var(Core_ast.Ident(name),None)
;;

let rec pattern_of_nested_pattern p =
  match p with
  | Nested_ast.Record_pattern(_,elements) ->
    Core_ast.Record_pattern (Ident_map.map pattern_of_nested_pattern elements)
  | Nested_ast.Fun_pattern(_) -> Core_ast.Fun_pattern
  | Nested_ast.Ref_pattern(_) -> Core_ast.Ref_pattern
  | Nested_ast.Int_pattern(_) -> Core_ast.Int_pattern
  | Nested_ast.Bool_pattern(_,b) -> Core_ast.Bool_pattern(b)
  | Nested_ast.String_pattern(_) -> Core_ast.String_pattern
;;

let rec function_value_of_nested_function_value
    (Nested_ast.Function(_,Nested_ast.Nested_var(_,x'),e')) =
  let (body,_) = clauses_and_var_of_nested_expr e' in
  Core_ast.Function_value(Core_ast.Var(x',None),Core_ast.Expr body)

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
       Core_ast.Value_body(
         Core_ast.Value_record(Core_ast.Record_value record_body)))
    | Nested_ast.Function_expr(_,f) ->
      ([], Core_ast.Value_body(
          Core_ast.Value_function(
            function_value_of_nested_function_value f)))
    | Nested_ast.Int_expr(_,n) ->
      ([], Core_ast.Value_body(Core_ast.Value_int(n)))
    | Nested_ast.Bool_expr(_,b) ->
      ([], Core_ast.Value_body(Core_ast.Value_bool(b)))
    | Nested_ast.String_expr(_,s) ->
      ([], Core_ast.Value_body(Core_ast.Value_string(s)))
    | Nested_ast.Ref_expr(_,e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      (cls0, Core_ast.Value_body(Core_ast.Value_ref(Core_ast.Ref_value(x'))))
    | Nested_ast.Var_expr(_,Nested_ast.Nested_var(_,x')) ->
      ([], Core_ast.Var_body(Core_ast.Var(x',None)))
    | Nested_ast.Appl_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      (cls1 @ cls2, Core_ast.Appl_body(x1,x2))
    | Nested_ast.Conditional_expr(_,e',p,f1,f2) ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Core_ast.Conditional_body(
          x', pattern_of_nested_pattern p,
          function_value_of_nested_function_value f1,
          function_value_of_nested_function_value f2))
    | Nested_ast.Deref_expr(_,e') ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Core_ast.Deref_body(x'))
    | Nested_ast.Update_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Core_ast.Update_body(x1,x2)
      )
    | Nested_ast.Binary_operation_expr(_,e1,op,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Core_ast.Binary_operation_body(x1,op,x2)
      )
    | Nested_ast.Unary_operation_expr(_,op,e1) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      (cls1, Core_ast.Unary_operation_body(op,x1))
    | Nested_ast.Indexing_expr(_,e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @ cls2
      , Core_ast.Indexing_body(x1,x2)
      )
    | Nested_ast.Let_expr(_,Nested_ast.Nested_var(_,x'),e1,e2) ->
      let (cls1,x1) = clauses_and_var_of_nested_expr e1 in
      let (cls2,x2) = clauses_and_var_of_nested_expr e2 in
      ( cls1 @
        [ Core_ast.Clause(Core_ast.Var(x',None), Core_ast.Var_body(x1)) ] @
        cls2
      , Core_ast.Var_body(x2)
      )
    | Nested_ast.Projection_expr(_,e',i) ->
      let (cls0,x') = clauses_and_var_of_nested_expr e' in
      ( cls0
      , Core_ast.Projection_body(x', i)
      )
  in
  (clauses @ [Core_ast.Clause(x,final_body)],x)

let a_translate_nested_expr e =
  let (cls,_) = clauses_and_var_of_nested_expr e in
  Core_ast.Expr cls
;;
