(**
  This module defines basic data types for the stack of a PDS.
*)
open Batteries;;

(** The type of stack actions which may be performed in a PDS. *)
type ( 'stack_element
     , 'targeted_dynamic_pop_action
     ) pds_stack_action =
  | Push of 'stack_element
    (** Represents the push of a single stack element. *)
  | Pop of 'stack_element
    (** Represents the pop of a single stack element. *)
  | Nop
    (** Represents no action being taken on the stack. *)
  | Pop_dynamic_targeted of 'targeted_dynamic_pop_action
    (** Represents a pop operation which leads to the target node only after
        performing a series of stack actions.  These stack actions are not
        fixed; they vary depending upon the stack element which is provided.
        This operation may also be non-deterministic, providing several
        chains of operations to the same target. *)
  [@@deriving ord]
;;

(** Pretty-prints a stack action. *)
let pp_pds_stack_action pp_stack_element pp_targeted_dynamic_pop_action action =
  match action with
  | Push element -> Printf.sprintf "Push(%s)" (pp_stack_element element)
  | Pop element -> Printf.sprintf "Pop(%s)" (pp_stack_element element)
  | Nop -> "Nop"
  | Pop_dynamic_targeted action' ->
    Printf.sprintf "Pop_dynamic_targeted(%s)"
      (pp_targeted_dynamic_pop_action action')
;;

(** The type of a module which resolves dynamic pops. *)
module type Dynamic_pop_handler =
sig
  (** The type of stack element in the PDS. *)
  type stack_element
  
  (** The type of states in the PDS. *)
  type state
  
  (** The type of targeted dynamic pop actions in the PDS. *)
  type targeted_dynamic_pop_action
  
  (** The type of untargeted dynamic pop actions in the PDS. *)
  type untargeted_dynamic_pop_action
  
  (** A type alias for stack actions in this handler. *)
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action
  
  (** A comparator for dynamic pop actions. *)
  val compare_targeted_dynamic_pop_action :
    targeted_dynamic_pop_action -> targeted_dynamic_pop_action -> int
  
  (** A pretty-printing function for targeted dynamic pop actions. *)
  val pp_targeted_dynamic_pop_action : targeted_dynamic_pop_action -> string
  
  (** A comparator for dynamic pop actions. *)
  val compare_untargeted_dynamic_pop_action :
    untargeted_dynamic_pop_action -> untargeted_dynamic_pop_action -> int
  
  (** A pretty-printing function for targeted dynamic pop actions. *)
  val pp_untargeted_dynamic_pop_action : untargeted_dynamic_pop_action -> string
  
  (** The resolution function for targeted dynamic pops.  This function takes a
      stack element which was pushed and the associated dynamic pop action.  The
      result is an enumeration of stack action sequences.  Each sequence is
      added to the PDS such that it starts at the source of the push and ends
      at the target of the dynamic pop. *)
  val perform_targeted_dynamic_pop :
    stack_element -> targeted_dynamic_pop_action ->
      stack_action list Enum.t

  (** The resolution function for untargeted dynamic pops.  This function takes
      a stack element which was pushed and the associated dynamic pop action.
      The result is an enumeration of pairs between stack action sequences and
      their eventual target. *)
  val perform_untargeted_dynamic_pop :
    stack_element -> untargeted_dynamic_pop_action ->
      (stack_action list * state) Enum.t
end;;

(** A module which serves as a dummy dynamic pop handler.  This handler should
    be used when no dynamic pops are required of the PDS. *)
module Null_dynamic_pop_hanlder(Basis : Pds_reachability_basis.Basis)
  : Dynamic_pop_handler
      with type stack_element = Basis.stack_element
      and type state = Basis.state
  =
struct
  type stack_element = Basis.stack_element;;
  type state = Basis.state;;
  type targeted_dynamic_pop_action = Null_targeted_action;;
  type untargeted_dynamic_pop_action = Null_untargeted_action;;
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action
  let compare_targeted_dynamic_pop_action
        Null_targeted_action Null_targeted_action = 0;;
  let pp_targeted_dynamic_pop_action Null_targeted_action = "_";;
  let compare_untargeted_dynamic_pop_action
        Null_untargeted_action Null_untargeted_action = 0;;
  let pp_untargeted_dynamic_pop_action Null_untargeted_action = "_";;
  let perform_targeted_dynamic_pop _ Null_targeted_action = Enum.empty ();;
  let perform_untargeted_dynamic_pop _ Null_untargeted_action = Enum.empty ();;
end;;
