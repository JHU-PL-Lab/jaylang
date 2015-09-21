(**
   This module defines the actual PDS reachability analysis.
*)

open Batteries;;

let logger = Logger_utils.make_logger "Pds_reachability_analysis";;
let lazy_logger = Logger_utils.make_lazy_logger "Pds_reachability_analysis";;

module type Analysis =
sig
  include Pds_reachability_types.Types;;

  (** The type of edge-generating functions used in this analysis. *)
  type edge_function = state -> (stack_action list * state) Enum.t

  exception Reachability_request_for_non_start_state of state;;

  (** The type of a reachability analysis in this module. *)
  type analysis

  (** The empty analysis.  This analysis has no states, edges, or edge
      functions. *)
  val empty : analysis

  (** Adds a single edge to a reachability analysis. *)
  val add_edge
    : state -> stack_action list -> state -> analysis -> analysis

  (** Adds a function to generate edges for a reachability analysis.  Given a
      source node, this function generates edges from that source edge.  The
      function must be pure; for a given source node, it must generate all edges
      that it can generate on the first call. *)
  val add_edge_function
    : (state -> (stack_action list * state) Enum.t)
    -> analysis
    -> analysis

  (** Adds a state and initial stack element to the analysis.  This permits the
      state to be used as the source state of a call to [get_reachable_states].
  *)
  val add_start_state
    : state
    -> stack_element
    -> analysis
    -> analysis

  (** Determines the states which are reachable from a given state and initial
      stack element.  This state must have been added to the analysis
      previously. *)
  val get_reachable_states
    : state
    -> stack_element
    -> analysis
    -> state Enum.t

  (** Pretty-printing function for the analysis. *)
  val pp_analysis : analysis -> string
  
  (** An exception raised when a reachable state query occurs before the state
      is added as a start state. *)
  exception Reachability_request_for_non_start_state of state;;
end;;

module Make(Basis : Pds_reachability_basis.Basis)
  : Analysis
  with type state = Basis.state
  and type stack_element = Basis.stack_element =
struct
  (********** Create and wire in appropriate components. **********)
  
  module Types = Pds_reachability_types.Make(Basis);;
  module Structure = Pds_reachability_structure.Make(Basis)(Types);;

  include Types;;

  type edge_function = state -> (stack_action list * state) Enum.t;;

  (********** Define utility data structures. **********)
  
  exception Reachability_request_for_non_start_state of state;;

  module State_set = Set.Make(Basis.State_ord);;

  module Node_ord =
  struct
    type t = Types.node
    let compare = Types.compare_node
  end;;

  module Node_set = Set.Make(Node_ord);;

  (********** Define analysis structure. **********)
  
  type analysis =
    { known_nodes : Node_set.t (** Nodes appearing somewhere in the structure *)
    ; live_nodes : Node_set.t (** Known starting nodes for lookups *)
    ; reachability : Structure.structure
    ; edge_functions : edge_function list
    };;

  (********** Define analysis operations. **********)
  
  let pp_node_set node_set =
    String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pp_node @@
      Node_set.enum node_set
  ;;
  
  let pp_analysis analysis =
    "{\n" ^
    (String_utils.indent 2 @@ String_utils.concat_sep ",\n" @@ List.enum
      [ "known nodes = " ^ pp_node_set analysis.known_nodes
      ; "live_nodes = " ^ pp_node_set analysis.live_nodes
      ; "reachability = " ^ Structure.pp_structure analysis.reachability
      ; "length edge_functions = " ^
          string_of_int (List.length analysis.edge_functions)
      ]) ^ "\n}"
  ;;

  let empty =
    { known_nodes = Node_set.empty
    ; live_nodes = Node_set.empty
    ; reachability = Structure.empty
    ; edge_functions = []
    };;
  
  (** Adds a "real" edge (the PDS reachability structure's edge, not the
      analysis interface's presentation of an edge) to an analysis.  Then,
      performs edge closure on the analysis. *)
  let rec add_real_edge_and_close edge analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () -> "add_real_edge_and_close " ^ pp_edge edge)
      (fun _ -> "Finished") @@
    fun () ->
    (* If we already have this edge, ignore the addition.  In particular, we can
       get an infinite loop (resulting in stack overflow) if we allow the
       closure to proceed. *)
    if Structure.has_edge edge analysis.reachability then analysis else
      (* First, add this edge to the lookup structures. *)
      let analysis' =
        { analysis with
          reachability = Structure.add_edge edge analysis.reachability
        }
      in
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
        | Nop _ | Push _ ->
          add_node edge.source false @@ add_node edge.target false analysis'
        | Pop _ -> analysis'
      in
      (* Next, we need to perform edge closure.  The particular action depends
         on this new edge's action.  The closure rules are summarized below.
          1. (a) -- push a --> (b) -- pop a --> (c) ==> (a) -- nop --> (c)
          2. (a) -- push a --> (b) -- nop --> (c) ==> (a) -- push a --> (c)
          3. (a) -- live nop --> (b) -- nop --> (c) ==> (a) -- live nop --> (c)
      *)
      let more_edges =
        match edge.edge_action with
        | Nop liveness ->
          (* Actual liveness may be modified by fixed live nodes. *)
          let liveness' =
            liveness || Node_set.mem edge.source analysis''.live_nodes
          in
          let push_into_source =
            analysis''.reachability
            |> Structure.find_push_edges_by_target edge.source
            |> Enum.map
              (fun (source',element) ->
                 { source = source'; target = edge.target
                 ; edge_action = Push element
                 })
          in
          let nop_into_source =
            analysis''.reachability
            |> Structure.find_live_nop_edges_by_target edge.source
            |> Enum.map
              (fun source' ->
                { source = source'; target = edge.target
                ; edge_action = Nop true })
          in
          let nop_out_of_target =
            if not liveness' then Enum.empty () else
              analysis''.reachability
              |> Structure.find_nop_edges_by_source edge.target
              |> Enum.map
                (fun target' ->
                  { source = edge.source; target = target'
                  ; edge_action = Nop true })
          in
          Enum.concat @@ List.enum
            [ push_into_source; nop_into_source; nop_out_of_target ]
        | Push element as act ->
          let nop_out_of_target =
            analysis''.reachability
            |> Structure.find_nop_edges_by_source edge.target
            |> Enum.map
              (fun target' ->
                 { source = edge.source; target = target'; edge_action = act })
          in
          let pop_out_of_target =
            analysis''.reachability
            |> Structure.find_pop_edges_by_source_and_element
              edge.target element
            |> Enum.map
              (fun target' ->
                let liveness = Node_set.mem edge.source analysis''.live_nodes in
                 { source = edge.source; target = target'
                ; edge_action = Nop liveness })
          in
          Enum.append nop_out_of_target pop_out_of_target
        | Pop element ->
          let push_into_source =
            analysis''.reachability
            |> Structure.find_push_edges_by_target_and_element
              edge.source element
            |> Enum.map
              (fun source' ->
                let liveness = Node_set.mem source' analysis''.live_nodes in
                { source = source'; target = edge.target
                ; edge_action = Nop liveness })
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
    let source_node =
      { node_state = source_state ; node_stack_left_to_push = [] }
    in 
    let (first_action, stack_left) =
      match stack_action_list with
      | [] ->
        let liveness = Node_set.mem source_node analysis.live_nodes in
        (Nop liveness, [])
      | action::actions' -> (action, actions')
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
  and add_node node live analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ ->
         "add_node (" ^ pp_node node ^ ") " ^ string_of_bool live)
      (fun _ -> "Finished") @@
    fun () ->
    (* If we already know about the node, then we need not do anything.  Note
       that, if the node is being marked live, we need to know that as well. *)
    let already_know = Node_set.mem node analysis.known_nodes in
    let liveness_ok = not live || Node_set.mem node analysis.live_nodes in
    if already_know && liveness_ok then analysis else
      (* We should start by adding the node to the structure. *)
      let analysis' =
        { analysis with
          known_nodes = Node_set.add node analysis.known_nodes;
          live_nodes = if live then Node_set.add node analysis.live_nodes else analysis.live_nodes
        }
      in
      (* Next, add edges for each action indicated by the node. *)
      let node_stack = node.node_stack_left_to_push in
      let analysis'' =
        match node_stack with
        | [] ->
          analysis.edge_functions
          |> List.enum
          |> Enum.map (fun f -> f node.node_state)
          |> Enum.concat
          |> Enum.fold
            (fun analysis' (actions,target_state) ->
               add_edge node.node_state actions target_state analysis')
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
      in
      (* Finally, if we've just learned that the node is live, make sure that
         all of its nop edges are marked as live. *)
      analysis''.reachability
      |> Structure.find_nop_edges_by_source node
      |> Enum.fold
        (fun analysis''' target ->
          let edge =
            { source = node; target = target; edge_action = Nop true }
          in
          add_real_edge_and_close edge analysis''')
        analysis''
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
    in add_node node true analysis
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
      analysis.reachability
      |> Structure.find_live_nop_edges_by_source node
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
