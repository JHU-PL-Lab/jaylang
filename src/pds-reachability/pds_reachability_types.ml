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
    | Pop of stack_element
    | Nop of bool
      (** A stack no-op.  Boolean indicates liveness for closure. *)

  (** Comparison for stack actions. *)
  val compare_stack_action : stack_action -> stack_action -> int

  (** A pretty-printer for stack actions. *)  
  val pp_stack_action : stack_action -> string  

  (** The type of node used for reachability. *)
  type node =
    { node_state : state
    ; node_stack_left_to_push : stack_action list
    };;

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
    | Nop of bool
        [@@deriving ord]
  ;;

  let pp_stack_action action =
    match action with
    | Push x -> "push " ^ Basis.pp_stack_element x
    | Pop x -> "pop " ^ Basis.pp_stack_element x
    | Nop x -> if x then "live-nop" else "nop"
  ;;

  type node =
    { node_state : state
    ; node_stack_left_to_push : stack_action list
    } [@@deriving ord];;

  let pp_node node =
    "(" ^ Basis.pp_state node.node_state ^ "," ^
    String_utils.pretty_list pp_stack_action node.node_stack_left_to_push ^ ")"
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