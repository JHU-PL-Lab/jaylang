(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Core_ast;;
open Core_ast_wellformedness;;
open Core_interpreter;;
open Ddpa_abstract_ast;;

(* TODO: new form of result type *)
(** Represents the result of processing an expression in the toploop. *)
type result =
  {
    illformednesses : illformedness list;
    (** A set of ill-formednesses discovered in the expression.  If this set is
        non-empty, then the remaining components of the result will be empty. *)

    analyses : ((string * string option * string list option) *
                Abs_filtered_value_set.t) list;
    (** An association list from each requested variable analysis to the
        possible values of that variable under those conditions.  If no
        analyses were requested or if the expression was ill-formed, this
        list will be empty. *)

    errors : Nested_toploop_analysis_types.error list;
    (** A list of the errors discovered in the provided expression by the
        core toploop analysis.  If no error checking was requested or if the
        expression was ill-formed, this list will be empty. *)

    evaluation_result : Core_toploop_types.evaluation_result
    (** The result of evaluating the provided expression.  This will be absent
        if evaluation was disabled, if error checking was enabled and discovered
        errors, or if the expression was ill-formed. *)
  }
;;

(** A record containing the callbacks that the toploop calls during evaluation.
    This allows for a more interactive experience than if the caller waits for
    the entire result to be produced. *)
type callbacks =
  { cb_illformednesses : Core_ast_wellformedness.illformedness list -> unit
  ; cb_variable_analysis :
      string -> string option -> string list option ->
      Abs_filtered_value_set.t -> unit
  ; cb_errors : Nested_toploop_analysis_types.error list -> unit
  ; cb_evaluation_result : var -> value Environment.t -> unit
  ; cb_evaluation_failed : string -> unit
  ; cb_evaluation_disabled : unit -> unit
  ; cb_size_report_callback : int * int * int * int * int -> unit
  }
;;
