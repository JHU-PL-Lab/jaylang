(**
   This module defines basic data types for the PDS reachability implementation
   and utility functions for them.
*)
open Batteries;;
open Pds_reachability_types_stack;;

module type Types =
sig
  (** The type of states in the PDS. *)
  type state

  (** The type of stack elements in the PDS. *)
  type stack_element
  
  (** The type of targeted dynamic pop actions in the PDS. *)
  type targeted_dynamic_pop_action
  
  (** The type of untargeted dynamic pop actions in the PDS. *)
  type untargeted_dynamic_pop_action
  
  (** Stack actions which may be performed in the PDS. *)
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action

  (** A pretty-printer for stack actions. *)  
  val pp_stack_action : stack_action -> string  

  (** The type of node used for reachability. *)
  type node =
    | State_node of state
    | Intermediate_node of node * stack_action list

  (** A comparison for nodes. *)
  val compare_node : node -> node -> int

  (** Pretty-printing for nodes. *)
  val pp_node : node -> string

  (** The type of edge used in reachability. *)
  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_action
    };;

  (** Pretty-printing for edges. *)
  val pp_edge : edge -> string
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
  let compare_targeted_dynamic_pop_action =
    Dph.compare_targeted_dynamic_pop_action;;

  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action
    [@@deriving ord]
  ;;

  let pp_stack_action action =
    match action with
    | Push x -> "push " ^ Basis.pp_stack_element x
    | Pop x -> "pop " ^ Basis.pp_stack_element x
    | Nop -> "nop"
    | Pop_dynamic_targeted action ->
      "pop_dynamic_targeted " ^ Dph.pp_targeted_dynamic_pop_action action
  ;;

  type node =
    | State_node of state
    | Intermediate_node of node * stack_action list
    [@@deriving ord]
  ;;

  let rec pp_node node =
    match node with
    | State_node state -> Basis.pp_state state
    | Intermediate_node (node,stack_actions) ->
      "InterNode" ^
      String_utils.pretty_tuple pp_node
        (String_utils.pretty_list pp_stack_action)
        (node,stack_actions)
  ;;

  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_action
    };;

  let pp_edge edge =
    pp_node edge.source ^ " --[" ^ pp_stack_action edge.edge_action ^ "]--> " ^
    pp_node edge.target
  ;;
end;;