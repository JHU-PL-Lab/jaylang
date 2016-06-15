(**
   This module defines basic data types for the PDS reachability implementation
   and utility functions for them.
*)
open Batteries;;
open Pds_reachability_types_stack;;
open Pp_utils;;

module type Types =
sig
  (** The type of states in the PDS. *)
  type state

  val equal_state : state -> state -> bool;;

  (** The type of stack elements in the PDS. *)
  type stack_element

  val equal_stack_element : stack_element -> stack_element -> bool;;

  (** The type of targeted dynamic pop actions in the PDS. *)
  type targeted_dynamic_pop_action

  val equal_targeted_dynamic_pop_action :
    targeted_dynamic_pop_action -> targeted_dynamic_pop_action -> bool;;

  (** The type of untargeted dynamic pop actions in the PDS. *)
  type untargeted_dynamic_pop_action

  val equal_untargeted_dynamic_pop_action :
    untargeted_dynamic_pop_action -> untargeted_dynamic_pop_action -> bool;;
  val compare_untargeted_dynamic_pop_action :
    untargeted_dynamic_pop_action -> untargeted_dynamic_pop_action -> int;;
  val pp_untargeted_dynamic_pop_action :
    Format.formatter -> untargeted_dynamic_pop_action -> unit;;

  (** Stack actions which may be performed in the PDS. *)
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action

  (** A pretty-printer for stack actions. *)
  val pp_stack_action : stack_action pretty_printer
  val show_stack_action : stack_action -> string

  (** The type of node used for reachability. *)
  type node =
    | State_node of state
    | Intermediate_node of node * stack_action list

  (** A comparison for nodes. *)
  val compare_node : node -> node -> int
  val equal_node : node -> node -> bool

  (** Pretty-printing for nodes. *)
  val pp_node : node pretty_printer
  val show_node : node -> string

  (** The type of edge used in reachability. *)
  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_action
    };;

  val equal_edge : edge -> edge -> bool
  val compare_edge : edge -> edge -> int

  (** Pretty-printing for edges. *)
  val pp_edge : edge pretty_printer
  val show_edge : edge -> string
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with type stack_element = Basis.stack_element
      and type state = Basis.state
    )
  : Types with type stack_element = Basis.stack_element
           and type state = Basis.state
           and type targeted_dynamic_pop_action =
                 Dph.targeted_dynamic_pop_action
           and type untargeted_dynamic_pop_action =
                 Dph.untargeted_dynamic_pop_action
=
struct
  type state = Basis.state;;
  type stack_element = Basis.stack_element;;
  type targeted_dynamic_pop_action = Dph.targeted_dynamic_pop_action;;
  type untargeted_dynamic_pop_action = Dph.untargeted_dynamic_pop_action;;

  let compare_state = Basis.State_ord.compare;;
  let compare_stack_element = Basis.Stack_element_ord.compare;;
  let equal_state s s' = Basis.State_ord.compare s s' = 0;;
  let equal_stack_element e e' =
    Basis.Stack_element_ord.compare e e' = 0
  ;;
  let equal_targeted_dynamic_pop_action a a' =
    Dph.compare_targeted_dynamic_pop_action a a' = 0
  ;;
  let equal_untargeted_dynamic_pop_action a a' =
    Dph.compare_untargeted_dynamic_pop_action a a' = 0
  ;;
  let compare_untargeted_dynamic_pop_action =
    Dph.compare_untargeted_dynamic_pop_action
  ;;
  let pp_untargeted_dynamic_pop_action =
    Dph.pp_untargeted_dynamic_pop_action
  ;;
  let equal_stack_action =
    Pds_reachability_types_stack.equal_pds_stack_action
      equal_stack_element equal_targeted_dynamic_pop_action
  ;;
  open Basis;;
  open Dph;;
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action
    [@@deriving ord, show]
  ;;

  type node =
    | State_node of state
    | Intermediate_node of node * stack_action list
    [@@deriving eq, ord, show]
  ;;

  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_action
    }
    [@@deriving eq, ord, show]
  ;;
end;;
