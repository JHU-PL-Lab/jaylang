open A_translator;;
open Batteries;;
open Nested_toploop_types;;

let handle_expression
    ?core_callbacks:(core_callbacks=Core_toploop.no_op_callbacks)
    (conf : Core_toploop_options.configuration)
    (nested_expr : Nested_ast.expr)
  =
  let (core_expr, core_map) = a_translate_nested_expr nested_expr in
  let result =
    Core_toploop.handle_expression
      ~callbacks:core_callbacks
      conf core_expr
  in
  let nested_errors =
    List.of_enum @@
    Nested_toploop_analysis.batch_translation
      core_map @@ List.enum result.Core_toploop_types.errors
  in
  { illformednesses = result.Core_toploop_types.illformednesses
  ; analyses = result.Core_toploop_types.analyses
  ; errors = nested_errors
  ; evaluation_result = result.Core_toploop_types.evaluation_result
  }
;;
