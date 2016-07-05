open Batteries;;
open Swan_toploop_types;;

let handle_expression
    ?core_callbacks:(core_callbacks=Core_toploop.no_op_callbacks)
    (conf : Core_toploop_options.configuration)
    (swan_expr : Swan_ast.expr)
  =
  let (nested_expr, nested_map) = Swan_translator.swan_to_nested_translation swan_expr in
  let result =
    Nested_toploop.handle_expression
      ~core_callbacks:core_callbacks
      conf nested_expr
  in
  let swan_errors =
    List.of_enum @@
    Swan_toploop_analysis.batch_translation
      nested_map @@ List.enum result.Nested_toploop_types.errors
  in
  { illformednesses = result.Nested_toploop_types.illformednesses
  ; analyses = result.Nested_toploop_types.analyses
  ; errors = swan_errors
  ; evaluation_result = result.Nested_toploop_types.evaluation_result
  }
;;
