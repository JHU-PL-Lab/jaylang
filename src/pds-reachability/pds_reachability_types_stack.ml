(**
  This module defines basic data types for the stack of a PDS.
*)
open Batteries;;

(** The type of stack actions which may be performed in a PDS. *)
type ('stack_element, 'dynamic_pop_action) pds_stack_action =
  | Push of 'stack_element
    (** Represents the push of a single stack element. *)
  | Pop of 'stack_element
    (** Represents the pop of a single stack element. *)
  | Nop
    (** Represents no action being taken on the stack. *)
  | Pop_dynamic of 'dynamic_pop_action
    (** Represents a pop operation which leads to the target node only after
        performing a series of stack actions.  These stack actions are not
        fixed; they vary depending upon the stack element which is provided.
        This operation may also be non-deterministic, providing several
        chains of operations to the same target. *)
  [@@deriving ord]
;;

(** The type of a module which resolves dynamic pops. *)
module type Dynamic_pop_handler =
sig
  (** The type of stack element in the PDS. *)
  type stack_element
  
  (** The type of dynamic pop action in the PDS. *)
  type dynamic_pop_action
  
  (** A comparator for dynamic pop actions. *)
  val compare_dynamic_pop_action :
    dynamic_pop_action -> dynamic_pop_action -> int
  
  (** A pretty-printing function for dynamic pop actions. *)
  val pp_dynamic_pop_action : dynamic_pop_action -> string
  
  (** The resolution function for dynamic pops.  This function takes a stack
      element which was pushed and the associated dynamic pop action.  The
      result is an enumeration of stack action sequences.  Each sequence is
      added to the PDS such that it starts at the source of the push and ends
      at the target of the dynamic pop. *)
  val perform_dynamic_pop :
    stack_element -> dynamic_pop_action ->
      ((stack_element, dynamic_pop_action) pds_stack_action) list Enum.t
end;;

(** A module which serves as a dummy dynamic pop handler.  This handler should
    be used when no dynamic pops are required of the PDS. *)
module Null_dynamic_pop_hanlder(Basis : Pds_reachability_basis.Basis)
  : Dynamic_pop_handler =
struct
  type stack_element = Basis.stack_element
  type dynamic_pop_action = Null_pop_action
  let compare_dynamic_pop_action Null_pop_action Null_pop_action = 0;;
  let pp_dynamic_pop_action Null_pop_action = "_";;
  let perform_dynamic_pop _ Null_pop_action = Enum.empty ();;
end;;
