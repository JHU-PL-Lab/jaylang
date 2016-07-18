(* open Core_toploop_types;; *)
open Nested_toploop_types;;
open Nested_toploop_analysis_types;;

(** These callbacks perform no operation when called. *)
val no_op_callbacks : callbacks

(** These callbacks print messages to stdout when called. *)
val stdout_callbacks : callbacks

(** An error-reporting callback which prints messages to stdout. *)
val stdout_errors_callback : error list -> unit

(** This function processes a nested AST expression.  This function accepts
    callback functions which are invoked when various steps of the expression
    handler are completed.  If unspecified, the callbacks do nothing. *)
val handle_expression :
  ?callbacks:callbacks ->
  Core_toploop_options.configuration ->
  Nested_ast.expr ->
  result
