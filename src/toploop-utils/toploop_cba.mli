(**
  This module provides high-level operations to make use of a CBA module.  In
  particular, it adds the following behavior:
    - Analyses are stored behind a mutable reference, so these functions do not
      require the caller to track updated analysis values.
    - This module adds data structures and values which are relevant to the
      application of the CBA, such as detecting misuse of call sites. 
*)

open Analysis

include module type of Toploop_cba_types;;

val pp_inconsistency : inconsistency -> string;;

module Make : functor (A : Analysis_sig) -> CBA;;

(** Finds an appropriate context stack from a given textual name.  Raises
    [Not_found] if the name is invalid.  The string ["none"] produces 
    [None] as the stack. *)
val stack_from_name :
  string -> (module Analysis_context_stack.Context_stack) option
