open Swan_toploop_types;;

(** This function processes a Swan AST expression.  This function accepts
    callback functions which are invoked when various steps of the expression
    handler are completed.  If unspecified, the callbacks do nothing. *)
val handle_expression :
  ?core_callbacks:Core_toploop_types.callbacks ->
  Core_toploop_options.configuration ->
  Swan_ast.expr ->
  result
