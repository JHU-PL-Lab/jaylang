(**
   This module defines the actual PDS reachability analysis.
*)

open Batteries;;
open Pds_reachability_types_stack;;

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

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
      with type stack_element = Basis.stack_element)
  : Analysis
  with type state = Basis.state
  and type stack_element = Basis.stack_element
  and type dynamic_pop_action = Dph.dynamic_pop_action
  =
struct
  (********** Create and wire in appropriate components. **********)
  
  module Types = Pds_reachability_types.Make(Basis)(Dph);;
  module Structure = Pds_reachability_structure.Make(Basis)(Dph)(Types);;

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
    { known_nodes : Node_set.t
    ; reachability : Structure.structure
    ; edge_functions : edge_function list
    ; next_intermediate_node : int
    };;

  (********** Define analysis operations. **********)
  
  let pp_analysis analysis =
    let known_nodes_str =
      String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pp_node @@
        Node_set.enum analysis.known_nodes
    in
    "{\n" ^
    (String_utils.indent 2 @@ String_utils.concat_sep ",\n" @@ List.enum
      [ "known nodes = " ^ known_nodes_str
      ; "reachability = " ^ Structure.pp_structure analysis.reachability
      ; "length edge_functions = " ^
          string_of_int (List.length analysis.edge_functions)
      ; "next_intermediate_node = " ^
          string_of_int analysis.next_intermediate_node
      ]) ^ "\n}"
  ;;

  let empty =
    { known_nodes = Node_set.empty
    ; reachability = Structure.empty
    ; edge_functions = []
    ; next_intermediate_node = 0
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
        | Nop | Push _ -> add_node edge.source @@ add_node edge.target analysis'
        | Pop _ | Pop_dynamic _ -> analysis'
      in
      (* Next, we need to perform edge closure.  The particular action depends
         on this new edge's action.  The closure rules are summarized below.
          1. (a) -- push a --> (b) -- pop a --> (c) ==> (a) -- nop --> (c)
          2. (a) -- push a --> (b) -- nop --> (c) ==> (a) -- push a --> (c)
          3. (a) -- nop --> (b) -- nop --> (c) ==> (a) -- nop --> (c)
          4. (a) -- push a --> (b) -- dynamic_pop f --> (c) ==>
              (a) -- action_1 --> ... -- action_n --> (c)
             for each [action_1,...,action_n] in f a
      *)
      (* more_edges is an enumeration containing edges which are immediately
         known *)
      let more_edges =
        match edge.edge_action with
        | Nop ->
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
            |> Structure.find_nop_edges_by_target edge.source
            |> Enum.map
              (fun source' ->
                { source = source'; target = edge.target; edge_action = Nop })
          in
          let nop_out_of_target =
            analysis''.reachability
            |> Structure.find_nop_edges_by_source edge.target
            |> Enum.map
              (fun target' ->
                { source = edge.source; target = target'; edge_action = Nop })
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
                 { source = edge.source; target = target'; edge_action = Nop })
          in
          Enum.append nop_out_of_target pop_out_of_target
        | Pop element ->
          let push_into_source =
            analysis''.reachability
            |> Structure.find_push_edges_by_target_and_element
              edge.source element
            |> Enum.map
              (fun source' ->
                 { source = source'; target = edge.target; edge_action = Nop })
          in
          push_into_source
        | Pop_dynamic _ -> Enum.empty ()
      in
      (* analysis_updates is an enumeration of functions which will update the
         analysis by adding edges.  This is appropriate when e.g. new
         intermediate nodes must be created. *)
      let analysis_updates =
        match edge.edge_action with
        | Nop -> Enum.empty ()
        | Push element ->
          let dynamic_pop_out_of_target =
            analysis''.reachability
            |> Structure.find_dynamic_pop_edges_by_source edge.target
            |> Enum.map
              (fun (target', action) ->
                fun (analysis : analysis) ->
                  Dph.perform_dynamic_pop element action
                  |> Enum.fold
                      (fun analysis' actions ->
                        add_edges_between_nodes
                          edge.source target' actions analysis')
                      analysis
              )
          in
          dynamic_pop_out_of_target
        | Pop _ -> Enum.empty ()
        | Pop_dynamic action ->
          let push_into_source =
            analysis''.reachability
            |> Structure.find_push_edges_by_target edge.source
            |> Enum.map
              (fun (source, element) ->
                fun analysis ->
                  Dph.perform_dynamic_pop element action
                  |> Enum.fold
                      (fun analysis' actions ->
                        add_edges_between_nodes
                          source edge.target actions analysis')
                      analysis
              )
          in
          push_into_source
      in
      let analysis_after_edges =
        more_edges
        |> Enum.fold (flip add_real_edge_and_close) analysis''
      in
      let analysis_after_updates =
        analysis_updates
        |> Enum.fold (fun a f -> f a) analysis_after_edges
      in
      analysis_after_updates

  (**
     Adds a sequence of edges between two nodes in the analysis.  This is
     accomplished by introducing intermediate nodes as necessary.
  *)
  and add_edges_between_nodes source_node target_node stack_actions analysis =
    (* This recursive function creates the chain of single stack action edges
       from the provided list of stack actions.  It consumes intermediate node
       numbers as necessary to generate the edges, so it takes and returns this
       value. *)
    let rec mk_real_edges actions next_intermediate next_source =
      match actions with
      | [] ->
        let edge =
          { source = next_source
          ; target = target_node
          ; edge_action = Nop
          }
        in
        ([edge], next_intermediate)
      | [action] ->
        let edge =
          { source = next_source
          ; target = target_node
          ; edge_action = action
          }
        in
        ([edge], next_intermediate)
      | action::actions' ->
        let target = Intermediate_node(next_intermediate) in
        let edge =
          { source = next_source
          ; target = target
          ; edge_action = action
          }
        in
        let (real_edges, next_intermediate') =
          mk_real_edges actions' (next_intermediate+1) target
        in
        (edge :: real_edges, next_intermediate')
    in
    (* Let's figure out which new single-action edges we will be adding. *)
    let (real_edges, next_intermediate') =
      mk_real_edges
        stack_actions
        analysis.next_intermediate_node
        source_node
    in
    let analysis' =
      { analysis with next_intermediate_node = next_intermediate' }
    in
    let analysis'' =
      add_node source_node @@ add_node target_node analysis'
    in
    (* Now, let's add them. *)
    real_edges
      |> List.fold_left
        (fun analysis''' real_edge ->
          add_real_edge_and_close real_edge analysis''')
        analysis''

  (**
     Adds an edge to this analysis.  Here, "edge" refers to the interface's
     presentation of an edge structure.
  *)
  and add_edge source_state stack_action_list target_state analysis =
    let source_node = State_node(source_state) in
    let target_node = State_node(target_state) in
    add_edges_between_nodes source_node target_node stack_action_list analysis

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
    match node with
    | Intermediate_node _ -> analysis
    | Initial_node _ ->
      if Node_set.mem node analysis.known_nodes then analysis else
        { analysis with
          known_nodes = Node_set.add node analysis.known_nodes }
    | State_node state ->
      if Node_set.mem node analysis.known_nodes then analysis else
        let analysis' =
          { analysis with
            known_nodes = Node_set.add node analysis.known_nodes }
        in
        analysis.edge_functions
        |> List.enum
        |> Enum.map (fun f -> f state)
        |> Enum.concat
        |> Enum.fold
          (fun analysis' (actions,target_state) ->
             add_edge state actions target_state analysis')
          analysis'
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
        match node with
        | State_node state -> Some state
        | Intermediate_node _ -> None
        | Initial_node _ -> None)
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
    analysis
    |> add_real_edge_and_close
        { source = Initial_node(state, stack_element)
        ; target = State_node(state)
        ; edge_action = Push stack_element
        }
  ;;

  let get_reachable_states state stack_element analysis =
    let node = Initial_node(state, stack_element) in
    if Node_set.mem node analysis.known_nodes
    then
      (*
        If a state is reachable by empty stack from the given starting state,
        then there will be a nop edge to it.  It's that simple by this point.
      *)
      analysis.reachability
      |> Structure.find_nop_edges_by_source node
      |> Enum.filter_map
        (fun node ->
          match node with
          | State_node state -> Some state
          | Intermediate_node _ -> None
          | Initial_node _ -> None
        )
    else
      raise @@ Reachability_request_for_non_start_state state  
  ;;
end;;
