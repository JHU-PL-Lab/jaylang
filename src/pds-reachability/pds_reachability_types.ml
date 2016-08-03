(**
   This module defines basic data types for the PDS reachability implementation
   and utility functions for them.
*)
open Batteries;;
open Pds_reachability_types_stack;;
open Pp_utils;;
open Pds_reachability_utils;;

module type Types =
sig
  (** The decorated type of states in the PDS. *)
  module State : Decorated_type

  (** The decorated type of stack elements in the PDS. *)
  module Stack_element : Decorated_type

  (** The decorated type of targeted dynamic pop actions in the PDS. *)
  module Targeted_dynamic_pop_action : Decorated_type

  (** The decorated type of untargeted dynamic pop actions in the PDS. *)
  module Untargeted_dynamic_pop_action : Decorated_type

  (** Stack actions which may be performed in the PDS. *)
  type stack_action =
    ( Stack_element.t
    , Targeted_dynamic_pop_action.t
    ) pds_stack_action

  val pp_stack_action : stack_action pretty_printer
  val show_stack_action : stack_action -> string
  val stack_action_to_yojson : stack_action -> Yojson.Safe.json

  type node =
    | State_node of State.t
    | Intermediate_node of node * stack_action list

  (** The decorated type of node used for reachability. *)
  module Node : Decorated_type
    with type t = node

  type edge =
    { source : Node.t
    ; target : Node.t
    ; edge_action : stack_action
    };;

  (** The decorated type of edge used in reachability. *)
  module Edge : Decorated_type
    with type t = edge
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State
    )
  : Types with module Stack_element = Basis.Stack_element
           and module State = Basis.State
           and module Targeted_dynamic_pop_action =
                 Dph.Targeted_dynamic_pop_action
           and module Untargeted_dynamic_pop_action =
                 Dph.Untargeted_dynamic_pop_action
=
struct
  module State = Basis.State;;
  module Stack_element = Basis.Stack_element;;
  module Targeted_dynamic_pop_action = Dph.Targeted_dynamic_pop_action;;
  module Untargeted_dynamic_pop_action = Dph.Untargeted_dynamic_pop_action;;

  let equal_stack_action =
    Pds_reachability_types_stack.equal_pds_stack_action
      Stack_element.equal Targeted_dynamic_pop_action.equal
  ;;
  type stack_action =
    ( Stack_element.t
    , Targeted_dynamic_pop_action.t
    ) pds_stack_action
    [@@deriving ord, show, to_yojson]
  ;;

  type node =
    | State_node of State.t
    | Intermediate_node of node * stack_action list
    [@@deriving eq, ord, show, to_yojson]
  ;;

  module Node =
  struct
    type t = node
    let equal = equal_node
    let compare = compare_node
    let pp = pp_node
    let show = show_node
    let to_yojson = node_to_yojson
  end;;

  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_action
    }
    [@@deriving eq, ord, show, to_yojson]
  ;;

  module Edge =
  struct
    type t = edge
    let equal = equal_edge
    let compare = compare_edge
    let pp = pp_edge
    let show = show_edge
    let to_yojson = edge_to_yojson
  end;;
end;;
