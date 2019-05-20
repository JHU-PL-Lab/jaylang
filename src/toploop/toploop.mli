(**
   This module defines the primary functions for the toploop.
*)

open Odefa_ast;;
open Odefa_ddpa;;
open Odefa_interpreter;;

open Ast;;
open Interpreter;;
open Toploop_analysis_types;;
open Toploop_types;;
open Ddpa_abstract_ast;;

(** This function processes a AST expression.  This function accepts
    callback functions which are invoked when various steps of the expression
    handler are completed.  If unspecified, the callbacks do nothing. *)
val handle_expression :
  ?callbacks:callbacks ->
  Toploop_options.configuration ->
  Ast.expr ->
  result

(** These callbacks perform no operation when called. *)
val no_op_callbacks : callbacks

(** These callbacks print messages to stdout when called. *)
val stdout_callbacks : callbacks

(** An illformedness callback which prints messages to stdout. *)
val stdout_illformednesses_callback :
  Ast_wellformedness.illformedness list -> unit

(** A variable analysis callback which prints messages to stdout. *)
val stdout_variable_analysis_callback :
  string -> string option -> string list option -> Abs_filtered_value_set.t ->
  unit

(** An error-reporting callback which prints messages to stdout. *)
val stdout_errors_callback : error list -> unit

(** An evaluation callback which prints messages to stdout. *)
val stdout_evaluation_result_callback : var -> value Environment.t -> unit

(** An evaluation callback which prints failure messages to stdout *)
val stdout_evaluation_failed_callback : string -> unit

(** A callback for the event in which evaluation is disabled which prints a
    message to stdout. *)
val stdout_evaluation_disabled_callback : unit -> unit

(** A routine that determines the PDA behind the analysis is *)
val stdout_size_report_callback : int * int * int * int * int -> unit
