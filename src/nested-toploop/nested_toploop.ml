open A_translator;;

open Nested_toploop_types;;

let handle_expression
    ?core_callbacks:(core_callbacks=Core_toploop.no_op_callbacks)
    (conf : Core_toploop_options.configuration)
    (nested_expr : Nested_ast.expr)
  =
  let core_expr = a_translate_nested_expr nested_expr in
  let result =
    Core_toploop.handle_expression
      ~callbacks:core_callbacks
      conf core_expr
  in
  (* TODO: This is where we should add resugaring of errors and similar
     things. *)
  { illformednesses = result.Core_toploop_types.illformednesses
  ; analyses = result.Core_toploop_types.analyses
  ; errors = result.Core_toploop_types.errors
  ; evaluation_result = result.Core_toploop_types.evaluation_result
  }
;;
