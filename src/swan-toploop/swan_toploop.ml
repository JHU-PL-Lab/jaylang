open Swan_toploop_types;;

let handle_expression
    ?core_callbacks:(core_callbacks=Core_toploop.no_op_callbacks)
    (conf : Core_toploop_options.configuration)
    (swan_expr : Swan_ast.expr)
  =
  let nested_expr = Swan_translator.translate_swan_expr_to_nested swan_expr in
  let result =
    Nested_toploop.handle_expression
      ~core_callbacks:core_callbacks
      conf nested_expr
  in
  (* TODO: This is where we should add resugaring of errors and similar
     things. *)
  { illformednesses = result.Nested_toploop_types.illformednesses
  ; analyses = result.Nested_toploop_types.analyses
  ; errors = result.Nested_toploop_types.errors
  ; evaluation_result = result.Nested_toploop_types.evaluation_result
  }
;;
