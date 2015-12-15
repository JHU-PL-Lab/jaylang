(**
   This module defines a data structure used in a PDS reachability analysis.
*)
open Batteries;;
open Pds_reachability_types_stack;;

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
  
  (** The type of targeted dynamic pop actions in this structure. *)
  type targeted_dynamic_pop_action

  (** The type of untargeted dynamic pop actions in this structure. *)
  type untargeted_dynamic_pop_action
  
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
  
  (** Adds an untargeted dynamic pop action to a reachability structure. *)
  val add_untargeted_dynamic_pop_action :
    node -> untargeted_dynamic_pop_action -> structure -> structure

  (** Determines if a given untargeted dynamic pop action is present in a
      reachability structure. *)
  val has_untargeted_dynamic_pop_action :
    node -> untargeted_dynamic_pop_action -> structure -> bool

  (** {6 Query functions.} *)

  val find_push_edges_by_source
    : node -> structure -> (node * stack_element) Enum.t
  val find_pop_edges_by_source
    : node -> structure -> (node * stack_element) Enum.t
  val find_nop_edges_by_source
    : node -> structure -> node Enum.t
  val find_targeted_dynamic_pop_edges_by_source
    : node -> structure -> (node * targeted_dynamic_pop_action) Enum.t
  val find_untargeted_dynamic_pop_actions_by_source
    : node -> structure -> untargeted_dynamic_pop_action Enum.t
  val find_push_edges_by_target
    : node -> structure -> (node * stack_element) Enum.t
  val find_pop_edges_by_target
    : node -> structure -> (node * stack_element) Enum.t
  val find_nop_edges_by_target
    : node -> structure -> node Enum.t
  val find_push_edges_by_source_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_pop_edges_by_source_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_push_edges_by_target_and_element
    : node -> stack_element -> structure -> node Enum.t
  val find_pop_edges_by_target_and_element
    : node -> stack_element -> structure -> node Enum.t
    
  val enumerate_nodes : structure -> node Enum.t
  val enumerate_edges : structure -> edge Enum.t
  
  (** {6 Submodules.} *)
  
  module Node_ord : Interfaces.OrderedType with type t = node
  module Node_set : Set.S with type elt = node
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
      with type stack_element = Basis.stack_element
      and type state = Basis.state)
    (Types : Pds_reachability_types.Types
      with type state = Basis.state
      and type stack_element = Basis.stack_element
      and type targeted_dynamic_pop_action = Dph.targeted_dynamic_pop_action
      and type untargeted_dynamic_pop_action = Dph.untargeted_dynamic_pop_action
      )
  : Structure
    with type stack_element = Basis.stack_element
     and type edge = Types.edge
     and type node = Types.node
     and type targeted_dynamic_pop_action = Types.targeted_dynamic_pop_action
     and type untargeted_dynamic_pop_action =
      Types.untargeted_dynamic_pop_action
=
struct
  (********** Wiring in the arguments. **********)
  open Basis;;
  open Dph;;
  open Types;;

  let compare_stack_element = Stack_element_ord.compare;;

  type stack_element = Basis.stack_element;;
  type node = Types.node;;
  type edge = Types.edge;;
  type targeted_dynamic_pop_action = Types.targeted_dynamic_pop_action;;
  type untargeted_dynamic_pop_action = Types.untargeted_dynamic_pop_action;;

  (********** Simple internal data structures. **********)

  type node_and_stack_element = node * stack_element [@@deriving ord];;

  type node_and_targeted_dynamic_pop_action =
    node * targeted_dynamic_pop_action [@@deriving ord]
  ;;

  let pp_node_and_stack_element x =
    String_utils.pretty_tuple Types.pp_node Basis.pp_stack_element x
  ;;

  let pp_node_and_targeted_dynamic_pop_action x =
    String_utils.pretty_tuple Types.pp_node Dph.pp_targeted_dynamic_pop_action x
  ;;

  (********** Substructure definitions. **********)

  module State_set = Set.Make(Basis.State_ord);;

  module Node_ord =
  struct
    type t = node
    let compare = compare_node
  end;;

  module Node_set = Set.Make(Node_ord);;

  module Node_map = Map.Make(Node_ord);;

  module Node_and_stack_element_ord =
  struct
    type t = node_and_stack_element
    let compare = compare_node_and_stack_element
  end;;

  module Node_and_targeted_dynamic_pop_action_ord =
  struct
    type t = node_and_targeted_dynamic_pop_action
    let compare = compare_node_and_targeted_dynamic_pop_action
  end;;

  module Untargeted_dynamic_pop_action_ord =
  struct
    type t = untargeted_dynamic_pop_action
    let compare = compare_untargeted_dynamic_pop_action
  end;;

  module Node_to_node_and_stack_element_multimap =
    Multimap.Make(Node_ord)(Node_and_stack_element_ord)
  ;;

  module Node_and_stack_element_to_node_multimap =
    Multimap.Make(Node_and_stack_element_ord)(Node_ord)
  ;;

  module Node_to_node_and_targeted_dynamic_pop_action_multimap =
    Multimap.Make(Node_ord)(Node_and_targeted_dynamic_pop_action_ord)
  ;;

  module Node_to_untargeted_dynamic_pop_action_multimap =
    Multimap.Make(Node_ord)(Untargeted_dynamic_pop_action_ord)
  ;;

  module Node_and_targeted_dynamic_pop_action_to_node_multimap =
    Multimap.Make(Node_and_targeted_dynamic_pop_action_ord)(Node_ord)
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

  module Node_to_node_and_targeted_dynamic_pop_action_multimap_pp =
    Multimap_pp.Make(
    struct
      module M = Node_to_node_and_targeted_dynamic_pop_action_multimap
      let pp_key = Types.pp_node
      let pp_value = pp_node_and_targeted_dynamic_pop_action
    end
    );;

  module Node_to_untargeted_dynamic_pop_action_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_to_untargeted_dynamic_pop_action_multimap
      let pp_key = Types.pp_node
      let pp_value = Dph.pp_untargeted_dynamic_pop_action
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
    ; targeted_dynamic_pop_edges_by_source : Node_to_node_and_targeted_dynamic_pop_action_multimap.t
    ; untargeted_dynamic_pop_actions_by_source : Node_to_untargeted_dynamic_pop_action_multimap.t
    ; push_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_target : Node_to_node_multimap.t
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
        ; "targeted_dynamic_pop_edges_by_source: " ^
          Node_to_node_and_targeted_dynamic_pop_action_multimap_pp.pp
            structure.targeted_dynamic_pop_edges_by_source
        ; "untargeted_dynamic_pop_actions_by_source: " ^
          Node_to_untargeted_dynamic_pop_action_multimap_pp.pp
            structure.untargeted_dynamic_pop_actions_by_source
        ; "push_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.push_edges_by_target
        ; "pop_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            structure.pop_edges_by_target
        ; "nop_edges_by_target: " ^
          Node_to_node_multimap_pp.pp
            structure.nop_edges_by_target
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
    ; targeted_dynamic_pop_edges_by_source =
        Node_to_node_and_targeted_dynamic_pop_action_multimap.empty
    ; untargeted_dynamic_pop_actions_by_source =
        Node_to_untargeted_dynamic_pop_action_multimap.empty
    ; push_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_target = Node_to_node_multimap.empty
    ; push_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; push_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    };;

  let has_edge edge analysis =
    match edge.edge_action with
    | Nop ->
      analysis.nop_edges_by_source
      |> Node_to_node_multimap.mem edge.source edge.target
    | Push element ->
      analysis.push_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
    | Pop element ->
      analysis.pop_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (edge.source, element) edge.target
    | Pop_dynamic_targeted action ->
      analysis.targeted_dynamic_pop_edges_by_source
      |> Node_to_node_and_targeted_dynamic_pop_action_multimap.mem
        edge.source (edge.target, action)
  ;;

  let add_edge edge structure =
    match edge.edge_action with
    | Nop ->
      { structure with
        nop_edges_by_source =
          structure.nop_edges_by_source
          |> Node_to_node_multimap.add edge.source edge.target
      ; nop_edges_by_target =
          structure.nop_edges_by_target
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
    | Pop_dynamic_targeted action ->
      { structure with
        targeted_dynamic_pop_edges_by_source =
          structure.targeted_dynamic_pop_edges_by_source
          |> Node_to_node_and_targeted_dynamic_pop_action_multimap.add
            edge.source (edge.target, action)
      }
  ;;

  let add_untargeted_dynamic_pop_action source action structure =
    { structure with
      untargeted_dynamic_pop_actions_by_source =
        structure.untargeted_dynamic_pop_actions_by_source
        |> Node_to_untargeted_dynamic_pop_action_multimap.add
          source action
    }
  ;;

  let has_untargeted_dynamic_pop_action source action structure =
    structure.untargeted_dynamic_pop_actions_by_source
    |> Node_to_untargeted_dynamic_pop_action_multimap.mem source action
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

  let find_targeted_dynamic_pop_edges_by_source source structure =
    Node_to_node_and_targeted_dynamic_pop_action_multimap.find source
      structure.targeted_dynamic_pop_edges_by_source
  ;;

  let find_untargeted_dynamic_pop_actions_by_source source structure =
    Node_to_untargeted_dynamic_pop_action_multimap.find source
      structure.untargeted_dynamic_pop_actions_by_source
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

  let enumerate_nodes structure =
    Node_set.enum @@ Node_set.of_enum @@ Enum.concat @@ List.enum
      [ Node_to_node_and_stack_element_multimap.keys
          structure.push_edges_by_source
      ; Node_to_node_and_stack_element_multimap.keys
          structure.pop_edges_by_source
      ; Node_to_node_multimap.keys
          structure.nop_edges_by_source
      ; Node_to_node_and_targeted_dynamic_pop_action_multimap.keys
          structure.targeted_dynamic_pop_edges_by_source
      ; Node_to_untargeted_dynamic_pop_action_multimap.keys
          structure.untargeted_dynamic_pop_actions_by_source
      ; Node_to_node_and_stack_element_multimap.keys
          structure.push_edges_by_target
      ; Node_to_node_and_stack_element_multimap.keys
          structure.pop_edges_by_target
      ; Node_to_node_multimap.keys
          structure.nop_edges_by_target
      ; Enum.map (fst % snd)
          (Node_to_node_and_targeted_dynamic_pop_action_multimap.enum
            structure.targeted_dynamic_pop_edges_by_source)
      ]
  ;;

  let enumerate_edges structure =
    Enum.concat @@ List.enum
      [ Node_to_node_and_stack_element_multimap.enum
          structure.push_edges_by_source
        |> Enum.map
            (fun (source,(target,element)) ->
              { source = source
              ; target = target
              ; edge_action = Push element
              })
      ; Node_to_node_and_stack_element_multimap.enum
          structure.pop_edges_by_source
        |> Enum.map
            (fun (source,(target,element)) ->
              { source = source
              ; target = target
              ; edge_action = Pop element
              })
      ; Node_to_node_multimap.enum
          structure.nop_edges_by_source
        |> Enum.map
            (fun (source,target) ->
              { source = source
              ; target = target
              ; edge_action = Nop
              })
      ; Node_to_node_and_targeted_dynamic_pop_action_multimap.enum
          structure.targeted_dynamic_pop_edges_by_source
        |> Enum.map
            (fun (source,(target,dynamic_pop)) ->
              { source = source
              ; target = target
              ; edge_action = Pop_dynamic_targeted dynamic_pop
              })
      ]
  ;;
end;;
