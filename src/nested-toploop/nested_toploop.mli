open Core_toploop_types;;
open Nested_toploop_types;;

(** This function processes a nested AST expression.  This function accepts
    callback functions which are invoked when various steps of the expression
    handler are completed.  If unspecified, the callbacks do nothing. *)
val handle_expression :
  ?core_callbacks:callbacks ->
  Core_toploop_options.configuration ->
  Nested_ast.expr ->
  result
