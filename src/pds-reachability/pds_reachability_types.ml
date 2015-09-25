(**
   This module defines basic data types for the PDS reachability implementation
   and utility functions for them.
*)
open Batteries;;

module type Types =
sig
  (** The type of states in the PDS. *)
  type state

  (** The type of stack elements in the PDS. *)
  type stack_element

  (** Stack actions which may be performed in the PDS. *)
  type stack_action =
    | Push of stack_element
      (** Represents the push of a single stack element. *)
    | Pop of stack_element
      (** Represents the pop of a single stack element. *)
    | Nop
      (** Represents no action being taken on the stack. *)

  (** A pretty-printer for stack actions. *)  
  val pp_stack_action : stack_action -> string  

  (** The type of node used for reachability. *)
  type node =
    | State_node of state
    | Intermediate_node of int
    | Initial_node of state * stack_element

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

module Make(Basis : Pds_reachability_basis.Basis)
  : Types with type stack_element = Basis.stack_element
           and type state = Basis.state =
struct
  type state = Basis.state
  type stack_element = Basis.stack_element

  let compare_state = Basis.State_ord.compare;;
  let compare_stack_element = Basis.Stack_element_ord.compare;;

  type stack_action =
    | Push of stack_element
    | Pop of stack_element
    | Nop
  ;;

  let pp_stack_action action =
    match action with
    | Push x -> "push " ^ Basis.pp_stack_element x
    | Pop x -> "pop " ^ Basis.pp_stack_element x
    | Nop -> "nop"
  ;;

  type node =
    | State_node of state
    | Intermediate_node of int
    | Initial_node of state * stack_element
    [@@deriving ord]
  ;;

  let pp_node node =
    match node with
    | State_node state -> Basis.pp_state state
    | Intermediate_node n -> "#" ^ string_of_int n
    | Initial_node(state,stack_element) ->
      String_utils.pretty_tuple Basis.pp_state Basis.pp_stack_element
        (state,stack_element)
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