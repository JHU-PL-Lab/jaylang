(**
  This module provides high-level operations to make use of a DDPA module.  In
  particular, it adds the following behavior:
    - Analyses are stored behind a mutable reference, so these functions do not
      require the caller to track updated analysis values.
    - This module adds data structures and values which are relevant to the
      application of the DDPA, such as detecting misuse of call sites.
*)

open Ddpa_analysis

include module type of Toploop_ddpa_types;;

val pp_inconsistency : Format.formatter -> inconsistency -> unit;;
val show_inconsistency : inconsistency -> string;;

module Make : functor (A : Analysis_sig) -> DDPA;;

(** Finds an appropriate context stack from a given textual name.  Raises
    [Not_found] if the name is invalid.  The string ["none"] produces
    [None] as the stack. *)
val stack_from_name :
  string -> (module Ddpa_context_stack.Context_stack) option
