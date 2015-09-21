(**
   This module defines a data structure used in a PDS reachability analysis.
*)
open Batteries;;

(**
   The type of the module which defines the data structure used within the
   analysis.
*)
module type Structure =
sig
  (** The stack element type used by the PDS. *)
  type stack_element

  (** The node type in the reachability structure. *)
  type node

  (** The edge type in the reachability structure. *)
  type edge

  (** The type of the PDS reachability data structure. *)
  type structure

  (** A pretty printer for the PDS reachability structure. *)
  val pp_structure : structure -> string

  (** The empty PDS reachability structure. *)
  val empty : structure

  (** Adds an edge to a reachability structure. *)
  val add_edge : edge -> structure -> structure

  (** Determines if a structure has a particular edge. *)
  val has_edge : edge -> structure -> bool

  (** {6 Query functions.} *)

  val find_push_edges_by_source
    : node -> structure -> (node * stack_element) Enum.t
  val find_pop_edges_by_source
    : node -> structure -> (node * stack_element) Enum.t
  val find_nop_edges_by_source
    : node -> structure -> node Enum.t
  val find_live_nop_edges_by_source
    : node -> structure -> node Enum.t
  val find_push_edges_by_target
    : node -> structure -> (node * stack_element) Enum.t
  val find_pop_edges_by_target
    : node -> structure -> (node * stack_element) Enum.t
  val find_nop_edges_by_target
    : node -> structure -> node Enum.t
  val find_live_nop_edges_by_target
    : node -> structure -> node Enum.t
  val find_push_edges_by_source_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_pop_edges_by_source_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_push_edges_by_target_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_pop_edges_by_target_and_element
    : node -> stack_element -> structure -> node Enum.t

end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Types : Pds_reachability_types.Types with type state = Basis.state and type stack_element = Basis.stack_element)
  : Structure
    with type stack_element = Basis.stack_element
     and type edge = Types.edge
     and type node = Types.node
=
struct
  (********** Wiring in the arguments. **********)
  open Basis;;
  open Types;;

  let compare_stack_element = Stack_element_ord.compare;;

  type stack_element = Basis.stack_element;;
  type node = Types.node;;
  type edge = Types.edge;;

  (********** Simple internal data structures. **********)

  type node_and_stack_element = node * stack_element [@@ deriving ord]
  ;;

  let pp_node_and_stack_element x =
    String_utils.pretty_tuple Types.pp_node Basis.pp_stack_element x
  ;;

  (********** Substructure definitions. **********)

  module State_set = Set.Make(Basis.State_ord);;

  module Node_ord =
  struct
    type t = node
    let compare = compare_node
  end;;

  module Node_set = Set.Make(Node_ord);;

  module Node_and_stack_element_ord =
  struct
    type t = node_and_stack_element
    let compare = compare_node_and_stack_element
  end;;

  module Node_to_node_and_stack_element_multimap =
    Multimap.Make(Node_ord)(Node_and_stack_element_ord)
  ;;

  module Node_and_stack_element_to_node_multimap =
    Multimap.Make(Node_and_stack_element_ord)(Node_ord)
  ;;

  module Node_to_node_multimap =
    Multimap.Make(Node_ord)(Node_ord)
  ;;

  module Node_to_node_and_stack_element_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_to_node_and_stack_element_multimap
      let pp_key = Types.pp_node
      let pp_value = pp_node_and_stack_element
    end
    );;

  module Node_and_stack_element_to_node_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_and_stack_element_to_node_multimap
      let pp_key = pp_node_and_stack_element
      let pp_value = Types.pp_node
    end
    );;

  module Node_to_node_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_to_node_multimap
      let pp_key = Types.pp_node
      let pp_value = Types.pp_node
    end
    );;

  (********** The data structure itself. **********)

  type structure =
    { push_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_source : Node_to_node_multimap.t
    ; live_nop_edges_by_source : Node_to_node_multimap.t
    ; push_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_target : Node_to_node_multimap.t
    ; live_nop_edges_by_target : Node_to_node_multimap.t
    ; push_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    ; pop_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    ; push_edges_by_target_and_element : Node_and_stack_element_to_node_multimap.t
    ; pop_edges_by_target_and_element : Node_and_stack_element_to_node_multimap.t
    };;

  let pp_structure structure =
    let structure_str = String_utils.concat_sep "\n" @@ List.enum
        [ "push_edges_by_source: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.push_edges_by_source
        ; "pop_edges_by_source: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.pop_edges_by_source
        ; "nop_edges_by_source: " ^
          Node_to_node_multimap_pp.pp
            structure.nop_edges_by_source
        ; "live_nop_edges_by_source: " ^
          Node_to_node_multimap_pp.pp
            structure.live_nop_edges_by_source
        ; "push_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.push_edges_by_target
        ; "pop_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.pop_edges_by_target
        ; "nop_edges_by_target: " ^
          Node_to_node_multimap_pp.pp
            structure.nop_edges_by_target
        ; "live_nop_edges_by_target: " ^
          Node_to_node_multimap_pp.pp
            structure.live_nop_edges_by_target
        ; "push_edges_by_source_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            structure.push_edges_by_source_and_element
        ; "pop_edges_by_source_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            structure.pop_edges_by_source_and_element
        ; "push_edges_by_target_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            structure.push_edges_by_target_and_element
        ; "pop_edges_by_target_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            structure.pop_edges_by_target_and_element
        ]
    in
    "{\n" ^ String_utils.indent 2 structure_str ^ "\n}"
  ;;

  let empty =
    { push_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_source = Node_to_node_multimap.empty
    ; live_nop_edges_by_source = Node_to_node_multimap.empty
    ; push_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_target = Node_to_node_multimap.empty
    ; live_nop_edges_by_target = Node_to_node_multimap.empty
    ; push_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; push_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    };;

  let has_edge edge analysis =
    match edge.edge_action with
    | Nop liveness ->
      Node_to_node_multimap.mem edge.source edge.target @@
        if liveness
        then analysis.live_nop_edges_by_source
        else analysis.nop_edges_by_source
    | Push element ->
      analysis.push_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
    | Pop element ->
      analysis.pop_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
  ;;

  let add_edge edge structure =
    match edge.edge_action with
    | Nop liveness ->
      let structure' =
        { structure with
          nop_edges_by_source =
            structure.nop_edges_by_source
            |> Node_to_node_multimap.add edge.source edge.target
        ; nop_edges_by_target =
            structure.nop_edges_by_target
            |> Node_to_node_multimap.add edge.target edge.source
        }
      in
      if not liveness then structure' else
        { structure' with
          live_nop_edges_by_source =
            structure.live_nop_edges_by_source
            |> Node_to_node_multimap.add edge.source edge.target
        ; live_nop_edges_by_target =
            structure.live_nop_edges_by_target
            |> Node_to_node_multimap.add edge.target edge.source
        }
    | Push element ->
      { structure with
        push_edges_by_source =
          structure.push_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (edge.target, element)
      ; push_edges_by_target =
          structure.push_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (edge.source, element)
      ; push_edges_by_source_and_element =
          structure.push_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.source, element) edge.target
      ; push_edges_by_target_and_element =
          structure.push_edges_by_target_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.target, element) edge.source
      }
    | Pop element ->
      { structure with
        pop_edges_by_source =
          structure.pop_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (edge.target, element)
      ; pop_edges_by_target =
          structure.pop_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (edge.source, element)
      ; pop_edges_by_source_and_element =
          structure.pop_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.source, element) edge.target
      ; pop_edges_by_target_and_element =
          structure.pop_edges_by_target_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (edge.target, element) edge.source
      }
  ;;

  let find_push_edges_by_source source structure =
    Node_to_node_and_stack_element_multimap.find
      source structure.push_edges_by_source
  ;;

  let find_pop_edges_by_source source structure =
    Node_to_node_and_stack_element_multimap.find
      source structure.pop_edges_by_source
  ;;

  let find_nop_edges_by_source source structure =
    Node_to_node_multimap.find source structure.nop_edges_by_source
  ;;

  let find_live_nop_edges_by_source source structure =
    Node_to_node_multimap.find source structure.live_nop_edges_by_source
  ;;

  let find_push_edges_by_target target structure =
    Node_to_node_and_stack_element_multimap.find
      target structure.push_edges_by_target
  ;;

  let find_pop_edges_by_target target structure =
    Node_to_node_and_stack_element_multimap.find
      target structure.pop_edges_by_target
  ;;

  let find_nop_edges_by_target target structure =
    Node_to_node_multimap.find target structure.nop_edges_by_target
  ;;

  let find_live_nop_edges_by_target target structure =
    Node_to_node_multimap.find target structure.live_nop_edges_by_target
  ;;

  let find_push_edges_by_source_and_element source stack_element structure =
    Node_and_stack_element_to_node_multimap.find
      (source,stack_element) structure.push_edges_by_source_and_element
  ;;

  let find_pop_edges_by_source_and_element source stack_element structure =
    Node_and_stack_element_to_node_multimap.find
      (source,stack_element) structure.pop_edges_by_source_and_element
  ;;

  let find_push_edges_by_target_and_element target stack_element structure =
    Node_and_stack_element_to_node_multimap.find
      (target,stack_element) structure.push_edges_by_target_and_element
  ;;

  let find_pop_edges_by_target_and_element target stack_element structure =
    Node_and_stack_element_to_node_multimap.find
      (target,stack_element) structure.pop_edges_by_target_and_element
  ;;

end;;
