(**
   This module defines basic data types for the stack of a PDS.
*)
open Batteries;;
open Pds_reachability_utils;;

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
  [@@deriving eq, ord, show, to_yojson]
;;

(** The type of a module which resolves dynamic pops. *)
module type Dynamic_pop_handler =
sig
  (** The decorated type of stack elements in the PDS. *)
  module Stack_element : Decorated_type

  (** The decorated type of states in the PDS. *)
  module State : Decorated_type

  (** The decorated type of targeted dynamic pop actions in the PDS. *)
  module Targeted_dynamic_pop_action : Decorated_type

  (** The decoreated type of untargeted dynamic pop actions in the PDS. *)
  module Untargeted_dynamic_pop_action : Decorated_type

  (** A type alias for stack actions in this handler. *)
  type stack_action =
    ( Stack_element.t
    , Targeted_dynamic_pop_action.t
    ) pds_stack_action

  (** The resolution function for targeted dynamic pops.  This function takes a
      stack element which was pushed and the associated dynamic pop action.  The
      result is an enumeration of stack action sequences.  Each sequence is
      added to the PDS such that it starts at the source of the push and ends
      at the target of the dynamic pop. *)
  val perform_targeted_dynamic_pop :
    Stack_element.t -> Targeted_dynamic_pop_action.t ->
    stack_action list Enum.t

  (** The resolution function for untargeted dynamic pops.  This function takes
      a stack element which was pushed and the associated dynamic pop action.
      The result is an enumeration of pairs between stack action sequences and
      their eventual target. *)
  val perform_untargeted_dynamic_pop :
    Stack_element.t -> Untargeted_dynamic_pop_action.t ->
    (stack_action list * State.t) Enum.t
end;;

(** A module which serves as a dummy dynamic pop handler.  This handler should
    be used when no dynamic pops are required of the PDS. *)
module Null_dynamic_pop_hanlder(Basis : Pds_reachability_basis.Basis)
  : Dynamic_pop_handler
    with module Stack_element = Basis.Stack_element
     and module State = Basis.State
=
struct
  module Stack_element = Basis.Stack_element;;
  module State = Basis.State;;
  module Targeted_dynamic_pop_action =
  struct
    type t = Null [@@deriving eq, ord, show, to_yojson]
  end;;
  module Untargeted_dynamic_pop_action =
  struct
    type t = Null [@@deriving eq, ord, show, to_yojson]
  end;;
  type stack_action =
    ( Stack_element.t
    , Targeted_dynamic_pop_action.t
    ) pds_stack_action
  let perform_targeted_dynamic_pop _ Targeted_dynamic_pop_action.Null =
    Enum.empty ()
  ;;
  let perform_untargeted_dynamic_pop _ Untargeted_dynamic_pop_action.Null =
    Enum.empty ()
  ;;
end;;
