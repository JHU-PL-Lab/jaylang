(** A utility type declaration module. This is stored separately so that the
    types need not be re-declared on in an interface. *)

open Jhupllib
open Jayil
open Ddpa
open Jayil_interpreter
open Jay_statistics
open Ast
open Ast_pp
open Ddpa_abstract_ast
open Interpreter
open Source_statistics

(** Represents the result of evaluating an expression. This data type also
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

type variable_analysis =
  (string * string option * string list option) * Abs_value_set.t
(** Represents the information produced by a variable analysis. *)

let pp_variable_analysis =
  Pp_utils.pp_tuple
    (Pp_utils.pp_triple Format.pp_print_string
       (Pp_utils.pp_option Format.pp_print_string)
       (Pp_utils.pp_option @@ Pp_utils.pp_list Format.pp_print_string))
    Ddpa_abstract_ast.Abs_value_set.pp

type result = {
  illformednesses : Ast_wellformedness.illformedness list;
      (** A set of ill-formednesses discovered in the expression. If this set is
          non-empty, then the remaining components of the result will be empty. *)
  analyses : variable_analysis list;
      (** An association list from each requested variable analysis to the
          possible values of that variable under those conditions. If no
          analyses were requested or if the expression was ill-formed, this list
          will be empty. *)
  errors : Toploop_analysis_types.error list;
      (** A list of the errors discovered in the provided expression by the
          toploop analysis. If no error checking was requested or if the
          expression was ill-formed, this list will be empty. *)
  evaluation_result : evaluation_result;
      (** The result of evaluating the provided expression. This will be absent
          if evaluation was disabled, if error checking was enabled and
          discovered errors, or if the expression was ill-formed. *)
}
[@@deriving show]
(** Represents the result of processing an expression in the toploop. *)

type callbacks = {
  cb_illformednesses : Ast_wellformedness.illformedness list -> unit;
  cb_variable_analysis :
    string -> string option -> string list option -> Abs_value_set.t -> unit;
  cb_errors : Toploop_analysis_types.error list -> unit;
  cb_evaluation_result : var -> value Environment.t -> unit;
  cb_evaluation_failed : string -> unit;
  cb_evaluation_disabled : unit -> unit;
  cb_size_report_callback : int * int * int * int * int -> unit;
  cb_source_statistics_callback : source_statistics -> unit;
  cb_input : unit -> int;
}
(** A record containing the callbacks that the toploop calls during evaluation.
    This allows for a more interactive experience than if the caller waits for
    the entire result to be produced. *)
