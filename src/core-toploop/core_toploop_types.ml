(**
   A utility type declaration module.  This is stored separately so that the
   types need not be re-declared on in an interface.
*)

open Core_ast;;
open Core_ast_pp;;
open Core_interpreter;;
open Ddpa_graph;;

(** Represents the result of evaluating an expression.  This data type also
    captures exceptional cases and toploop configuration properties. *)
type evaluation_result =
  | Evaluation_completed of var * evaluation_environment
  (** The case in which evaluation was successful. *)

  | Evaluation_failure of string
  (** The case in which evaluation became stuck. *)

  | Evaluation_invalidated
  (** The case in which evaluation was not performed due to some kind of
      previous problem (e.g. a well-formedness error). *)

  | Evaluation_disabled
  (** The case in which the user specifically disabled evaluation. *)
  [@@deriving show]
;;

(** Represents the information produced by a variable analysis. *)
type variable_analysis =
  (string * string option * string list option) * Abs_filtered_value_set.t
;;
let pp_variable_analysis =
  Pp_utils.pp_tuple
    (Pp_utils.pp_triple
       Format.pp_print_string
       (Pp_utils.pp_option Format.pp_print_string)
       (Pp_utils.pp_option @@ Pp_utils.pp_list Format.pp_print_string))
    Ddpa_graph.pp_abs_filtered_value_set
;;

(** Represents the result of processing an expression in the toploop. *)
type result =
  {
    illformednesses : Core_ast_wellformedness.illformedness list;
    (** A set of ill-formednesses discovered in the expression.  If this set is
        non-empty, then the remaining components of the result will be empty. *)

    analyses : variable_analysis list;
    (** An association list from each requested variable analysis to the
        possible values of that variable under those conditions.  If no
        analyses were requested or if the expression was ill-formed, this
        list will be empty. *)

    errors : Core_toploop_analysis_types.error list;
    (** A list of the errors discovered in the provided expression by the
        core toploop analysis.  If no error checking was requested or if the
        expression was ill-formed, this list will be empty. *)

    evaluation_result : evaluation_result
    (** The result of evaluating the provided expression.  This will be absent
        if evaluation was disabled, if error checking was enabled and discovered
        errors, or if the expression was ill-formed. *)
  }
  [@@deriving show]
;;

(** A record containing the callbacks that the toploop calls during evaluation.
    This allows for a more interactive experience than if the caller waits for
    the entire result to be produced. *)
type callbacks =
  { cb_illformednesses : Core_ast_wellformedness.illformedness list -> unit
  ; cb_variable_analysis :
      string -> string option -> string list option ->
      Abs_filtered_value_set.t -> unit
  ; cb_errors : Core_toploop_analysis_types.error list -> unit
  ; cb_evaluation_result : var -> value Environment.t -> unit
  ; cb_evaluation_failed : string -> unit
  ; cb_evaluation_disabled : unit -> unit
  ; cb_size_report_callback : int * int * int * int * int -> unit
  }
;;
