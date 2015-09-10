(**
   This module is meant to test reachability in a push-down system which accepts
   by empty stack.
*)

open Batteries;;

open String_utils;;

let logger = Logger_utils.make_logger "Pds_reachability";;
let lazy_logger = Logger_utils.make_lazy_logger "Pds_reachability";;

module type Spec =
sig
  type state
  type stack_element

  module State_ord : Interfaces.OrderedType with type t = state
  module Stack_element_ord : Interfaces.OrderedType with type t = stack_element

  val pp_state : state -> string
  val pp_stack_element : stack_element -> string 
end;;

type 'a stack_action =
  | Push of 'a
  | Pop of 'a
  | Nop
      [@@deriving eq, ord]
;;

let pp_stack_action pp_stack_element action =
  match action with
  | Push x -> "push " ^ pp_stack_element x
  | Pop x -> "pop " ^ pp_stack_element x
  | Nop -> "nop"
;;

module type Sig =
sig
  type state
  type stack_element

  exception Reachability_request_for_non_start_state of state;;

  (** The type of a reachability analysis in this module. *)
  type reachability_analysis

  (** The empty analysis.  This analysis has no states, edges, or edge
      functions. *)
  val empty : reachability_analysis

  (** Adds a single edge to a reachability analysis. *)
  val add_edge
    : state
    -> stack_element stack_action list
    -> state
    -> reachability_analysis
    -> reachability_analysis

  (** Adds a function to generate edges for a reachability analysis.  Given a
      source node, this function generates edges from that source edge.  The
      function must be pure; for a given source node, it must generate all edges
      that it can generate on the first call. *)
  val add_edge_function
    : (state -> (stack_element stack_action list * state) Enum.t)
    -> reachability_analysis
    -> reachability_analysis

  (** Adds a state and initial stack element to the analysis.  This permits the
      state to be used as the source state of a call to [get_reachable_states].
  *)
  val add_start_state
    : state
    -> stack_element
    -> reachability_analysis
    -> reachability_analysis

  (** Determines the states which are reachable from a given state and initial
      stack element.  This state must have been added to the analysis
      previously. *)
  val get_reachable_states
    : state
    -> stack_element
    -> reachability_analysis
    -> state Enum.t

  (** Pretty-printing function for the analysis. *)
  val pp_analysis : reachability_analysis -> string
end;;

module Make(S : Spec)
  : Sig with type state = S.state and type stack_element = S.stack_element =
struct
  type state = S.state
  type stack_element = S.stack_element

  exception Reachability_request_for_non_start_state of state;;

  type edge_function = state -> (stack_element stack_action list * state) Enum.t

  let compare_state = S.State_ord.compare
  let compare_stack_element = S.Stack_element_ord.compare

  type node =
    { node_state : state
    ; node_stack_left_to_push : stack_element stack_action list
    } [@@deriving ord];;

  let pp_node node =
    "(" ^ S.pp_state node.node_state ^ "," ^
    pretty_list (pp_stack_action S.pp_stack_element)
      node.node_stack_left_to_push ^
    ")"
  ;;

  module State_set = Set.Make(S.State_ord);;

  module Node_ord =
  struct
    type t = node
    let compare = compare_node
  end;;

  module Node_set = Set.Make(Node_ord);;

  type edge =
    { source : node
    ; target : node
    ; edge_action : stack_element stack_action
    };;

  let pp_edge edge =
    pp_node edge.source ^ " --[" ^
    pp_stack_action S.pp_stack_element edge.edge_action ^ "]-->" ^
    pp_node edge.target
  ;;

  type node_and_stack_element =
    | Node_and_stack_element of node * stack_element
                                  [@@ deriving ord]
  ;;

  let pp_node_and_stack_element (Node_and_stack_element(node,stack_element)) =
    pretty_tuple pp_node S.pp_stack_element (node,stack_element)
  ;;

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
      let pp_key = pp_node
      let pp_value = pp_node_and_stack_element
    end
    );;

  module Node_and_stack_element_to_node_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_and_stack_element_to_node_multimap
      let pp_key = pp_node_and_stack_element
      let pp_value = pp_node
    end
    );;

  module Node_to_node_multimap_pp = Multimap_pp.Make(
    struct
      module M = Node_to_node_multimap
      let pp_key = pp_node
      let pp_value = pp_node
    end
    );;

  (* TODO: some indexing structure for edge functions to allow irrelevant ones
           to be skipped. *)
  type reachability_analysis =
    { known_nodes : Node_set.t
    ; push_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_source : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_source : Node_to_node_multimap.t
    ; push_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; pop_edges_by_target : Node_to_node_and_stack_element_multimap.t
    ; nop_edges_by_target : Node_to_node_multimap.t
    ; push_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    ; pop_edges_by_source_and_element : Node_and_stack_element_to_node_multimap.t
    ; push_edges_by_target_and_element : Node_and_stack_element_to_node_multimap.t
    ; pop_edges_by_target_and_element : Node_and_stack_element_to_node_multimap.t
    ; edge_functions : edge_function list
    };;

  let pp_analysis analysis =
    let known_nodes_str =
      concat_sep_delim "{" "}" ", " @@ Enum.map pp_node @@
      Node_set.enum analysis.known_nodes
    in
    let analysis_str = concat_sep "\n" @@ List.enum
        [ "known_nodes: " ^ known_nodes_str
        ; "push_edges_by_source: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            analysis.push_edges_by_source
        ; "pop_edges_by_source: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            analysis.pop_edges_by_source
        ; "nop_edges_by_source: " ^
          Node_to_node_multimap_pp.pp
            analysis.nop_edges_by_source
        ; "push_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            analysis.push_edges_by_target
        ; "pop_edges_by_target: " ^
          Node_to_node_and_stack_element_multimap_pp.pp
            analysis.pop_edges_by_target
        ; "nop_edges_by_target: " ^
          Node_to_node_multimap_pp.pp
            analysis.nop_edges_by_target
        ; "push_edges_by_source_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            analysis.push_edges_by_source_and_element
        ; "pop_edges_by_source_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            analysis.pop_edges_by_source_and_element
        ; "push_edges_by_target_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            analysis.push_edges_by_target_and_element
        ; "pop_edges_by_target_and_element: " ^
          Node_and_stack_element_to_node_multimap_pp.pp
            analysis.pop_edges_by_target_and_element
        ; "edge_functions: " ^ string_of_int (List.length analysis.edge_functions)
        ]
    in
    "{\n" ^ indent 2 analysis_str ^ "\n}"
  ;;

  let empty =
    { known_nodes = Node_set.empty
    ; push_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_source = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_source = Node_to_node_multimap.empty
    ; push_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; pop_edges_by_target = Node_to_node_and_stack_element_multimap.empty
    ; nop_edges_by_target = Node_to_node_multimap.empty
    ; push_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_source_and_element = Node_and_stack_element_to_node_multimap.empty
    ; push_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    ; pop_edges_by_target_and_element = Node_and_stack_element_to_node_multimap.empty
    ; edge_functions = []
    };;

  let has_real_edge edge analysis =
    match edge.edge_action with
    | Nop ->
      analysis.nop_edges_by_source
      |> Node_to_node_multimap.mem edge.source edge.target
    | Push element ->
      analysis.push_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (Node_and_stack_element(edge.source, element)) edge.target
    | Pop element ->
      analysis.pop_edges_by_source_and_element
      |> Node_and_stack_element_to_node_multimap.mem
        (Node_and_stack_element(edge.source, element)) edge.target
  ;;

  let add_real_edge_without_closing edge analysis =
    match edge.edge_action with
    | Nop ->
      { analysis with
        nop_edges_by_source =
          analysis.nop_edges_by_source
          |> Node_to_node_multimap.add edge.source edge.target
      ; nop_edges_by_target =
          analysis.nop_edges_by_target
          |> Node_to_node_multimap.add edge.target edge.source
      }
    | Push element ->
      { analysis with
        push_edges_by_source =
          analysis.push_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (Node_and_stack_element(edge.target, element))
      ; push_edges_by_target =
          analysis.push_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (Node_and_stack_element(edge.source, element))
      ; push_edges_by_source_and_element =
          analysis.push_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (Node_and_stack_element(edge.source, element)) edge.target
      ; push_edges_by_target_and_element =
          analysis.push_edges_by_target_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (Node_and_stack_element(edge.target, element)) edge.source
      }
    | Pop element ->
      { analysis with
        pop_edges_by_source =
          analysis.pop_edges_by_source
          |> Node_to_node_and_stack_element_multimap.add edge.source
            (Node_and_stack_element(edge.target, element))
      ; pop_edges_by_target =
          analysis.pop_edges_by_target
          |> Node_to_node_and_stack_element_multimap.add edge.target
            (Node_and_stack_element(edge.source, element))
      ; pop_edges_by_source_and_element =
          analysis.pop_edges_by_source_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (Node_and_stack_element(edge.source, element)) edge.target
      ; pop_edges_by_target_and_element =
          analysis.pop_edges_by_target_and_element
          |> Node_and_stack_element_to_node_multimap.add
            (Node_and_stack_element(edge.target, element)) edge.source
      }
  ;;

  (** Adds a "real" edge (the edge data structure, not the interface's
      presentation of an edge) to an analysis.  Then, performs edge closure on
      the analysis. *)
  let rec add_real_edge_and_close edge analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () -> "add_real_edge_and_close " ^ pp_edge edge)
      (fun _ -> "Finished") @@
    fun () ->
    (* If we already have this edge, ignore the addition.  In particular, we can
       get an infinite loop (resulting in stack overflow) if we allow the
       closure to proceed. *)
    if has_real_edge edge analysis then analysis else
      (* First, add this edge to the lookup structures. *)
      let analysis' = add_real_edge_without_closing edge analysis in
      (* Now that this edge is added, we need to consider other edges which may
         arise directly from its presence.  This edge introduces a target node;
         if we have not yet seen that target, we have not executed the edge
         functions on it.  We should do this for nodes of push and nop edges
         because they are "alive": there is necessarily a path from a starting
         node to reach them due to how edges are added to the analysis.  We do
         not do this for pop edges, instead waiting until the pop is canceled by
         a push during closure (see below). *)
      let analysis'' =
        match edge.edge_action with
        | Nop | Push _ -> add_node edge.source @@ add_node edge.target analysis'
        | Pop _ -> analysis'
      in
      (* Next, we need to perform edge closure.  The particular action depends
         on this new edge's action.  The closure rules are summarized below.
          1. (a) -- push a --> (b) -- pop a --> (c) ==> (a) -- nop --> (c)
          2. (a) -- push a --> (b) -- nop --> (c) ==> (a) -- push a --> (c)
          3. (a) -- nop --> (b) -- nop --> (c) ==> (a) -- nop --> (c)
         TODO: only perform closure rule #3 if node (a) was actively added to
               the analysis.  The only reason those edges are important is for
               answering the top-level reachability question; they are not
               necessary for correctness otherwise.
      *)
      let more_edges =
        match edge.edge_action with
        | Nop ->
          let push_into_source =
            analysis''.push_edges_by_target
            |> Node_to_node_and_stack_element_multimap.find edge.source
            |> Enum.map
              (fun (Node_and_stack_element(source',element)) ->
                 { source = source'; target = edge.target
                 ; edge_action = Push element
                 })
          in
          let nop_into_source =
            analysis''.nop_edges_by_target
            |> Node_to_node_multimap.find edge.source
            |> Enum.map
              (fun source' ->
                { source = source'; target = edge.target; edge_action = Nop })
          in
          let nop_out_of_target =
            analysis''.nop_edges_by_source
            |> Node_to_node_multimap.find edge.target
            |> Enum.map
              (fun target' ->
                { source = edge.source; target = target'; edge_action = Nop })
          in
          Enum.concat @@ List.enum
            [ push_into_source; nop_into_source; nop_out_of_target ]
        | Push element as act ->
          let nop_out_of_target =
            analysis''.nop_edges_by_source
            |> Node_to_node_multimap.find edge.target
            |> Enum.map
              (fun target' ->
                 { source = edge.source; target = target'; edge_action = act })
          in
          let pop_out_of_target =
            analysis''.pop_edges_by_source_and_element
            |> Node_and_stack_element_to_node_multimap.find
              (Node_and_stack_element (edge.target, element))
            |> Enum.map
              (fun target' ->
                 { source = edge.source; target = target'; edge_action = Nop })
          in
          Enum.append nop_out_of_target pop_out_of_target
        | Pop element ->
          let push_into_source =
            analysis''.push_edges_by_target_and_element
            |> Node_and_stack_element_to_node_multimap.find
              (Node_and_stack_element (edge.source, element))
            |> Enum.map
              (fun source' ->
                 { source = source'; target = edge.target; edge_action = Nop })
          in
          push_into_source
      in
      more_edges
      |> Enum.fold (flip add_real_edge_and_close) analysis''

  (**
     Adds an edge to this analysis.  Here, "edge" refers to the interface's
     presentation of an edge structure.
  *)
  and add_edge source_state stack_action_list target_state analysis =
    let (first_action, stack_left) =
      match stack_action_list with
      | [] -> (Nop, [])
      | action::actions' -> (action, actions')
    in
    let source_node =
      { node_state = source_state ; node_stack_left_to_push = [] }
    in 
    let target_node =
      { node_state = target_state ; node_stack_left_to_push = stack_left }
    in
    let edge = { source = source_node
               ; target = target_node
               ; edge_action = first_action
               } in
    add_real_edge_and_close edge analysis

  (**
     Adds a node to the analysis.  Specifically, adds all edges with that node
     as a source that are returned by the analysis's edge functions.
  *)
  and add_node node analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ ->
         "add_node (" ^ pp_node node ^ ")")
      (fun _ -> "Finished") @@
    fun () ->
    if Node_set.mem node analysis.known_nodes then analysis else
      let { node_state = node_state; node_stack_left_to_push = node_stack } =
        node
      in
      let analysis' =
        { analysis with
          known_nodes = Node_set.add node analysis.known_nodes }
      in
      match node_stack with
      | [] ->
        analysis.edge_functions
        |> List.enum
        |> Enum.map (fun f -> f node.node_state)
        |> Enum.concat
        |> Enum.fold
          (fun analysis' (actions,target_state) ->
             add_edge node_state actions target_state analysis')
          analysis'
      | action::actions ->
        let target = { node_state = node.node_state
                     ; node_stack_left_to_push = actions
                     } in
        let edge = { source = node
                   ; target = target
                   ; edge_action = action
                   } in
        add_real_edge_and_close edge analysis' 
  ;;

  let add_edge_function edge_function analysis =
    let analysis' =
      { analysis with edge_functions = edge_function::analysis.edge_functions }
    in
    (* Of course, there may be nodes already in the graph which will respond to
       this function.  Make sure to get edges for them. *)
    analysis'.known_nodes
    |> Node_set.enum
    |> Enum.filter_map
      (fun node ->
         if List.is_empty node.node_stack_left_to_push
         then Some node.node_state
         else None)
    |> Enum.map
      (fun source_state ->
         edge_function source_state |> Enum.map (fun x -> (source_state, x)))
    |> Enum.concat
    |> Enum.fold
      (fun analysis'' (source_state,(action_list,target_state)) ->
         add_edge source_state action_list target_state analysis'')
      analysis'
  ;;

  let add_start_state state stack_element analysis =
    let node = { node_state = state
               ; node_stack_left_to_push = [Push stack_element]
               }
    in add_node node analysis
  ;;

  let get_reachable_states state stack_element analysis =
    let node = { node_state = state
               ; node_stack_left_to_push = [Push stack_element]
               }
    in
    if Node_set.mem node analysis.known_nodes
    then
      (*
        If a state is reachable by empty stack from the given starting state,
        then there will be a nop edge to it.  It's that simple by this point.
      *)
      analysis.nop_edges_by_source
      |> Node_to_node_multimap.find node
      |> Enum.filter_map
        (fun node ->
           match node.node_stack_left_to_push with
           | [] -> Some node.node_state
           | _::_ -> None
        )
    else
      raise @@ Reachability_request_for_non_start_state state  
  ;;
end;;
