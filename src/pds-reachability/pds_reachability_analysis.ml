(**
   This module defines the actual PDS reachability analysis.
*)

open Batteries;;

open Pds_reachability_types_stack;;
open Pp_utils;;
open Yojson_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Pds_reachability_analysis";;

module type Analysis =
sig
  include Pds_reachability_types.Types;;

  (** The type of edge-generating functions used in this analysis. *)
  type edge_function = State.t -> (stack_action list * State.t) Enum.t

  (** The type of functions to generate untargeted dynamic pop actions in this
      analysis. *)
  type untargeted_dynamic_pop_action_function =
    State.t -> Untargeted_dynamic_pop_action.t Enum.t

  (** The type of a reachability analysis in this module. *)
  type analysis

  (** The empty analysis.  This analysis has no states, edges, or edge
      functions. *)
  val empty : analysis

  (** Adds a single edge to a reachability analysis. *)
  val add_edge
    : State.t -> stack_action list -> State.t -> analysis -> analysis

  (** Adds a function to generate edges for a reachability analysis.  Given a
      source node, the function generates edges from that source node.  The
      function must be pure; for a given source node, it must generate all edges
      that it can generate on the first call. *)
  val add_edge_function : edge_function -> analysis -> analysis

  (** Adds an untargeted pop action to a reachability analysis.  Untargeted pop
      action are similar to targeted pop actions except that they are not
      created as an edge with a target node; instead, the target is decided in
      some way by the pushed element that the untargeted dynamic pop is
      consuming. *)
  val add_untargeted_dynamic_pop_action
    : State.t -> Untargeted_dynamic_pop_action.t -> analysis -> analysis

  (** Adds a function to generate untargeted dynamic pop ations for a
      reachability analysis.  Given a source node, the function generates
      untargeted actions from that source node.  The function must be pure; for
      a given source, it must generate all actions that it can generate on the
      first call. *)
  val add_untargeted_dynamic_pop_action_function
    : untargeted_dynamic_pop_action_function -> analysis -> analysis

  (** Adds a state and initial stack element to the analysis.  This permits the
      state to be used as the source state of a call to [get_reachable_states].
  *)
  val add_start_state
    : State.t
    -> stack_action list
    -> analysis
    -> analysis

  (** Determines whether the reachability analysis is closed. *)
  val is_closed : analysis -> bool

  (** Takes a step toward closing a given reachability analysis.  If the
      analysis is already closed, it is returned unchanged. *)
  val closure_step : analysis -> analysis

  (** Fully closes the provided analysis. *)
  val fully_close : analysis -> analysis

  (** Determines the states which are reachable from a given state and initial
      stack element.  This state must have been added to the analysis
      previously.  If the analysis is not fully closed, then the enumeration of
      reachable states may be incomplete.  *)
  val get_reachable_states
    : State.t
    -> stack_action list
    -> analysis
    -> State.t Enum.t

  (** Pretty-printing function for the analysis. *)
  val pp_analysis : analysis pretty_printer
  val show_analysis : analysis -> string

  (** An exception raised when a reachable state query occurs before the state
      is added as a start state. *)
  exception Reachability_request_for_non_start_state of State.t;;

  (** Determines the size of the provided analysis in terms of both node and
      edge count (respectively). *)
  val get_size : analysis -> int * int

  (** Extracts a subset of information about an analysis state as JSON data.
      Some parts of the analysis state (such as edge functions) will be
      elided as they cannot be represented. *)
  val dump_yojson : analysis -> Yojson.Safe.json
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State)
    (Work_collection_template_impl :
       Pds_reachability_work_collection.Work_collection_template)
  : Analysis
    with module State = Basis.State
     and module Stack_element = Basis.Stack_element
     and module Targeted_dynamic_pop_action = Dph.Targeted_dynamic_pop_action
     and module Untargeted_dynamic_pop_action = Dph.Untargeted_dynamic_pop_action
=
struct
  (********** Create and wire in appropriate components. **********)

  module Types = Pds_reachability_types.Make(Basis)(Dph);;
  module Work = Pds_reachability_work.Make(Basis)(Types);;
  module Work_collection_impl = Work_collection_template_impl(Work);;
  module Structure = Pds_reachability_structure.Make(Basis)(Dph)(Types);;

  include Types;;

  type edge_function = State.t -> (stack_action list * State.t) Enum.t;;
  type untargeted_dynamic_pop_action_function =
    State.t -> Untargeted_dynamic_pop_action.t Enum.t;;

  (********** Define utility data structures. **********)

  exception Reachability_request_for_non_start_state of State.t;;

  module State_set = struct
    module Impl = Set.Make(Basis.State);;
    include Impl;;
    include Set_pp(Impl)(Basis.State);;
    include Set_to_yojson(Impl)(Basis.State);;
  end;;

  module Node_set = struct
    module Impl = Set.Make(Node);;
    include Impl;;
    include Set_pp(Impl)(Node);;
    include Set_to_yojson(Impl)(Node);;
  end;;

  module Node_map = struct
    module Impl = Map.Make(Node);;
    include Impl;;
    include Map_pp(Impl)(Node);;
    include Map_to_yojson(Impl)(Node);;
  end;;

  (********** Define analysis structure. **********)

  type node_awareness =
    | Seen
    (* Indicates that this node exists somewhere in the work queue but not in
       the analysis structure. *)
    | Expanded
    (* Indicates that this node exists somewhere in the analysis structure and
       has already been expanded.  An expanded node responds to edge functions,
       for instance. *)
    [@@deriving show, to_yojson]
  let _ = show_node_awareness;; (* To ignore an unused generated function. *)

  type analysis =
    { node_awareness_map : node_awareness Node_map.t
    (* A mapping from each node to whether the analysis is aware of it.  Any node
       not in this map has not been seen in any fashion.  Every node that has been
       seen will be mapped to an appropriate [node_awareness] value. *)
    ; known_states : State_set.t
    (* A collection of all states appearing somewhere within the reachability
       structure (whether they have been expanded or not). *)
    ; start_nodes : Node_set.t
    (* A collection of the nodes which are recognized starting points.  Nop
       closure is only valid when sourced from these nodes, so these are the
       only nodes from which reachability questions are appropriate. *)
    ; reachability : Structure.t
    (* The underlying structure maintaining the nodes and edges in the graph. *)
    ; edge_functions : edge_function list
          [@printer fun formatter functions ->
                 Format.fprintf formatter "(length = %d)"
                   (List.length functions)]
    (* The list of all edge functions for this analysis. *)
    ; untargeted_dynamic_pop_action_functions :
        untargeted_dynamic_pop_action_function list
          [@printer fun formatter functions ->
                 Format.fprintf formatter "(length = %d)"
                   (List.length functions)]
    (* The list of all untargeted dynamic pop action functions. *)
    ; work_collection : Work_collection_impl.work_collection
    (* The collection of work which has not yet been performed. *)
    }
    [@@deriving show]
  ;;

  (********** Analysis utility functions. **********)

  let add_work work analysis =
    match work with
    | Work.Expand_node node ->
      if Node_map.mem node analysis.node_awareness_map
      then analysis
      else
        { analysis with
          work_collection =
            Work_collection_impl.offer work analysis.work_collection
        ; node_awareness_map =
            Node_map.add node Seen analysis.node_awareness_map
        }
    | Work.Introduce_edge edge ->
      (* TODO: We might want to filter duplicate introduce-edge steps from the
         work collection. *)
      if Structure.has_edge edge analysis.reachability
      then analysis
      else
        { analysis with
          work_collection =
            Work_collection_impl.offer work analysis.work_collection
        }
    | Work.Introduce_untargeted_dynamic_pop(from_node,action) ->
      (* TODO: We might want to filter duplicate introduce-udynpop steps from
         the work collection. *)
      if Structure.has_untargeted_dynamic_pop_action
          from_node action analysis.reachability
      then analysis
      else
        { analysis with
          work_collection =
            Work_collection_impl.offer work analysis.work_collection
        }
  ;;

  let add_works works analysis = Enum.fold (flip add_work) analysis works;;

  let next_edge_in_sequence from_node actions to_node =
    let next_node,action =
      match actions with
      | [] -> to_node,Nop
      | [x] -> to_node,x
      | x::xs -> Intermediate_node(to_node,xs),x
    in
    {source=from_node;target=next_node;edge_action=action}
  ;;

  (********** Define analysis operations. **********)

  let get_size analysis =
    let reachability = analysis.reachability in
    let node_count = Enum.count @@ Structure.enumerate_nodes reachability in
    let edge_count = Enum.count @@ Structure.enumerate_edges reachability in
    (node_count, edge_count)
  ;;

  let empty =
    { node_awareness_map = Node_map.empty
    ; known_states = State_set.empty
    ; start_nodes = Node_set.empty
    ; reachability = Structure.empty
    ; edge_functions = []
    ; untargeted_dynamic_pop_action_functions = []
    ; work_collection = Work_collection_impl.empty
    };;

  let add_edge from_state stack_action_list to_state analysis =
    let from_node = State_node from_state in
    let to_node = State_node to_state in
    let edge = next_edge_in_sequence from_node stack_action_list to_node in
    analysis
    |> add_work (Work.Expand_node from_node)
    |> Option.apply
      (match edge.edge_action with
       | Pop _
       | Pop_dynamic_targeted _ -> None
       | _ -> Some (add_work (Work.Expand_node to_node)))
    |> add_work (Work.Introduce_edge edge)
  ;;

  let add_edge_function edge_function analysis =
    (* First, we have to catch up on this edge function by calling it with every
       state present in the analysis. *)
    let work =
      analysis.known_states
      |> State_set.enum
      |> Enum.map
        (fun from_state ->
           from_state
           |> edge_function
           |> Enum.map
             (fun (actions,to_state) ->
                let edge =
                  next_edge_in_sequence
                    (State_node from_state)
                    actions
                    (State_node to_state)
                in
                (* We know that the from_node has already been introduced. *)
                Work.Introduce_edge edge
             )
        )
      |> Enum.concat
    in
    (* Now we add both the catch-up work (so the analysis is as if the edge
       function was present all along) and the edge function (so it'll stay
       in sync in the future). *)
    { (add_works work analysis) with
      edge_functions = edge_function :: analysis.edge_functions
    }
  ;;

  let add_untargeted_dynamic_pop_action from_state pop_action analysis =
    let from_node = State_node from_state in
    analysis
    |> add_work (Work.Expand_node from_node)
    |> add_work (Work.Introduce_untargeted_dynamic_pop(from_node, pop_action))
  ;;

  let add_untargeted_dynamic_pop_action_function pop_action_fn analysis =
    (* First, we have to catch up on this function by calling it with every
       state we know about. *)
    let work =
      analysis.known_states
      |> State_set.enum
      |> Enum.map
        (fun from_state ->
           from_state
           |> pop_action_fn
           |> Enum.map
             (fun action ->
                let from_node = State_node from_state in
                Work.Introduce_untargeted_dynamic_pop(from_node, action)
             )
        )
      |> Enum.concat
    in
    (* Now we add both the catch-up work (so the analysis is as if the function
       was present all along) and the function (so it'll stay in sync in the
       future). *)
    { (add_works work analysis) with
      untargeted_dynamic_pop_action_functions =
        pop_action_fn :: analysis.untargeted_dynamic_pop_action_functions
    }
  ;;

  let add_start_state state stack_actions analysis =
    let start_node = Intermediate_node(State_node(state), stack_actions) in
    (* If we've not seen this state before, then we need to expand it.
       If we have, we need to consider nop closure for it to catch it up. *)
    let entry =
      Node_map.Exceptionless.find start_node analysis.node_awareness_map
    in
    if entry <> Some Expanded
    then
      (* This node isn't in our structure, so we just need to add it.  Nop
         expansion, edge generation, and so on will occur when the expansion
         work is actually done. *)
      { (analysis |> add_work (Work.Expand_node(start_node))) with
        start_nodes = analysis.start_nodes |> Node_set.add start_node
      }
    else
      (* We've seen this node before but it didn't used to be a start node.
         We need to perform nop closure on it now to maintain the invariant
         that start nodes are nop-closed.  We only need to perform one frontier
         of nop closure; the insertion of those new nop edges will begin the
         cascade of nop closure to re-establish the invariant. *)
      let edge_work =
        analysis.reachability
        |> Structure.find_nop_edges_by_source start_node
        |> Enum.map
          (fun middle_node ->
             analysis.reachability
             |> Structure.find_nop_edges_by_source middle_node
             |> Enum.map
               (fun target_node ->
                  Work.Introduce_edge
                    ({source=start_node;target=target_node;edge_action=Nop}))
          )
        |> Enum.concat
      in
      add_works edge_work analysis
  ;;

  let is_closed analysis =
    Work_collection_impl.is_empty analysis.work_collection
  ;;

  let closure_step analysis =
    let (new_work_collection, work_opt) =
      Work_collection_impl.take analysis.work_collection
    in
    match work_opt with
    | None -> analysis
    | Some work ->
      lazy_logger `trace
        (fun () ->
           Printf.sprintf "PDS reachability closure step: %s"
             (Work.show work)
        );
      let analysis = { analysis with work_collection = new_work_collection } in
      (* A utility function to add a node to a set *only if* it needs to be
         expanded. *)
      (* TODO: consider - do we want to do this filtering in add_work or
         something?  The advantage here is that we don't build up a big set...
      *)
      let expand_add node nodes_to_expand =
        let entry =
          Node_map.Exceptionless.find node analysis.node_awareness_map
        in
        if entry <> Some Expanded
        then Node_set.add node nodes_to_expand
        else nodes_to_expand
      in
      match work with
      | Work.Expand_node node ->
        begin
          (* We're adding to the analysis a node that it does not contain. *)
          match node with
          | State_node(state) ->
            (* We just need to introduce this node to all of the edge functions
               that we have accumulated so far. *)
            let edge_work =
              analysis.edge_functions
              |> List.enum
              |> Enum.map (fun f -> f state)
              |> Enum.concat
              |> Enum.map (fun (actions,to_state) ->
                  let edge =
                    next_edge_in_sequence
                      (State_node state)
                      actions
                      (State_node to_state)
                  in
                  (* We know that the from_node has already been introduced. *)
                  Work.Introduce_edge edge
                )
            in
            let popdynu_work =
              analysis.untargeted_dynamic_pop_action_functions
              |> List.enum
              |> Enum.map (fun f -> f state)
              |> Enum.concat
              |> Enum.map
                (fun action ->
                   Work.Introduce_untargeted_dynamic_pop(node, action)
                )
            in
            { (analysis |> add_works edge_work |> add_works popdynu_work) with
              known_states = analysis.known_states |> State_set.add state
            ; node_awareness_map =
                analysis.node_awareness_map
                |> Node_map.add node Expanded
            }
          | Intermediate_node(target, actions) ->
            (* The only edge implied by an intermediate node is the one that
               moves along the action chain. *)
            let edge =
              match actions with
              | [] -> {source=node;target=target;edge_action=Nop}
              | [action] -> {source=node;target=target;edge_action=action}
              | action::actions' ->
                { source=node
                ; target=Intermediate_node(target, actions')
                ; edge_action=action}
            in
            (* We now have some work based upon the node to introduce.  We must
               also mark the node as present. *)
            { (analysis
               |> add_work (Work.Introduce_edge edge)
               |> add_work (Work.Expand_node edge.target)
              ) with
              node_awareness_map =
                Node_map.add node Expanded analysis.node_awareness_map
            }
        end
      | Work.Introduce_edge edge ->
        let { source = from_node
            ; target = to_node
            ; edge_action = action
            } = edge
        in
        let analysis' =
          (* When an edge is introduced, all of the edges connecting to it should
             be closed with it.  These new edges are introduced to the work queue
             and drive the gradual expansion of closure.  It may also be necessary
             to expand some nodes which have not yet been expanded. *)
          let edge_work, nodes_to_expand =
            match action with
            | Nop ->
              (* The primary closure for nop edges is to find all pushes that
                 lead into them and route them through the nop.  As this creates
                 new push edges, the target should be expanded when at least one
                 edge is created. *)
              let push_closure_work, push_closure_nodes =
                let work =
                  analysis.reachability
                  |> Structure.find_push_edges_by_target from_node
                  |> Enum.map
                    (fun (from_node', element) ->
                       Work.Introduce_edge(
                         { source = from_node'
                         ; target = to_node
                         ; edge_action = Push element
                         })
                    )
                in
                if Enum.is_empty work
                then (work, Node_set.empty)
                else (work, expand_add to_node Node_set.empty)
              in
              (* A further closure for nop edges occurs when two nop edges are
                 adjacent *AND* the source of the first edge is a start node.
                 In that case, we perform nop closure.  Since the new edge is
                 effectively "live" -- there are no pops between its target and
                 a start node -- we should treat the targets of these new edges
                 as expansion candidates as well.  As with other closure
                 processes, this is done in two steps: from the perspective of
                 the left edge (where a right edge is added) and vice versa. *)
              let left_nop_work, left_nop_nodes =
                let work =
                  analysis.reachability
                  |> Structure.find_nop_edges_by_target from_node
                  |> Enum.filter_map
                    (fun from_node' ->
                       if Node_set.mem from_node' analysis.start_nodes
                       then Some (
                           Work.Introduce_edge(
                             { source = from_node'
                             ; target = to_node
                             ; edge_action = Nop
                             }))
                       else None
                    )
                in
                if Enum.is_empty work
                then (work, Node_set.empty)
                else (work, expand_add to_node Node_set.empty)
              in
              let right_nop_work, right_nop_nodes =
                if not @@ Node_set.mem from_node analysis.start_nodes
                then (Enum.empty(), Node_set.empty)
                else
                  let work_list, nodes =
                    analysis.reachability
                    |> Structure.find_nop_edges_by_source to_node
                    |> Enum.fold
                      (fun (work_list,nodes) to_node' ->
                         let work = Work.Introduce_edge(
                             { source = from_node
                             ; target = to_node'
                             ; edge_action = Nop
                             })
                         in
                         (work::work_list, expand_add to_node' nodes)
                      ) ([],Node_set.empty)
                  in (List.enum work_list, nodes)
              in
              (* Now put it all together. *)
              ( Enum.concat @@ List.enum
                  [ push_closure_work
                  ; left_nop_work
                  ; right_nop_work
                  ]
              , push_closure_nodes
                |> Node_set.union left_nop_nodes
                |> Node_set.union right_nop_nodes
                |> expand_add to_node
              )
            | Push k ->
              (* Any nop, pop, or popdyn edges at the target of this push can be
                 closed.  Any new targets are candidates for expansion. *)
              let nop_work_list, nop_expand_set =
                analysis.reachability
                |> Structure.find_nop_edges_by_source to_node
                |> Enum.fold
                  (fun (work_list, expand_set) to_node' ->
                     let work =
                       Work.Introduce_edge(
                         { source = from_node
                         ; target = to_node'
                         ; edge_action = Push k
                         })
                     in
                     (work::work_list, expand_add to_node' expand_set)
                  )
                  ([], Node_set.empty)
              in
              let pop_work_list, pop_expand_set =
                analysis.reachability
                |> Structure.find_pop_edges_by_source to_node
                |> Enum.filter
                  (fun (_, element) -> Stack_element.equal k element)
                |> Enum.fold
                  (fun (work_list, expand_set) (to_node', _) ->
                     let work =
                       Work.Introduce_edge(
                         { source = from_node
                         ; target = to_node'
                         ; edge_action = Nop
                         })
                     in
                     (work::work_list, expand_add to_node' expand_set)
                  )
                  ([], Node_set.empty)
              in
              let popdynt_work_list, popdynt_expand_set =
                analysis.reachability
                |> Structure.find_targeted_dynamic_pop_edges_by_source to_node
                |> Enum.fold
                  (fun (work_list, expand_set) (to_node', action) ->
                     Dph.perform_targeted_dynamic_pop k action
                     |> Enum.fold
                       (fun (work_list, expand_set) stack_actions ->
                          let edge =
                            next_edge_in_sequence
                              from_node stack_actions to_node'
                          in
                          let work = Work.Introduce_edge edge in
                          (work::work_list, expand_add to_node' expand_set)
                       ) (work_list,expand_set)
                  ) ([],Node_set.empty)
              in
              let popdynu_work_list, popdynu_expand_set =
                analysis.reachability
                |> Structure.find_untargeted_dynamic_pop_actions_by_source
                  to_node
                |> Enum.fold
                  (fun (work_list, expand_set) action ->
                     Dph.perform_untargeted_dynamic_pop k action
                     |> Enum.fold
                       (fun (work_list, expand_set) (stack_actions, to_state) ->
                          let to_node' = State_node to_state in
                          let edge =
                            next_edge_in_sequence
                              from_node stack_actions to_node'
                          in
                          let work = Work.Introduce_edge edge in
                          (work::work_list, expand_add to_node' expand_set)
                       ) (work_list, expand_set)
                  ) ([], Node_set.empty)
              in
              ( Enum.concat @@ List.enum
                  [ List.enum nop_work_list
                  ; List.enum pop_work_list
                  ; List.enum popdynt_work_list
                  ; List.enum popdynu_work_list
                  ]
              , nop_expand_set
                |> Node_set.union pop_expand_set
                |> Node_set.union popdynt_expand_set
                |> Node_set.union popdynu_expand_set
                |> expand_add to_node
              )
            | Pop k ->
              (* Pop edges can only close with the push edges that precede them.
                 The target of these new edges is a candidate for expansion. *)
              let work =
                analysis.reachability
                |> Structure.find_push_edges_by_target from_node
                |> Enum.filter_map
                  (fun (from_node', element) ->
                     if Stack_element.equal element k
                     then Some(
                         Work.Introduce_edge(
                           { source = from_node'
                           ; target = to_node
                           ; edge_action = Nop
                           }))
                     else None
                  )
              in
              if Enum.is_empty work
              then (work, Node_set.empty)
              else (work, expand_add to_node Node_set.empty)
            | Pop_dynamic_targeted action ->
              (* Dynamic pop edges can only close with push edges that precede
                 them.  The target of these new edges is a candidate for
                 expansion. *)
              let (work_list, to_expand) =
                analysis.reachability
                |> Structure.find_push_edges_by_target from_node
                |> Enum.fold
                  (fun (work_list, expand_set) (from_node', element) ->
                     Dph.perform_targeted_dynamic_pop element action
                     |> Enum.fold
                       (fun (work_list, expand_set) stack_actions ->
                          let edge =
                            next_edge_in_sequence
                              from_node' stack_actions to_node
                          in
                          let work = Work.Introduce_edge edge in
                          (work::work_list, expand_add to_node expand_set)
                       ) (work_list,expand_set)
                  ) ([],Node_set.empty)
              in (List.enum work_list, to_expand)
          in
          let expand_work = nodes_to_expand
                            |> Node_set.enum
                            |> Enum.map (fun node -> Work.Expand_node node)
          in
          add_works (Enum.append edge_work expand_work) analysis
        in
        { analysis' with
          reachability = analysis'.reachability |> Structure.add_edge edge
        }
      | Work.Introduce_untargeted_dynamic_pop(from_node,action) ->
        (* Untargeted dynamic pops can only close with the push edges that
           reach them.  Any targets of the resulting edges are candidates for
           expansion. *)
        let analysis' =
          let (work_list, nodes_to_expand) =
            analysis.reachability
            |> Structure.find_push_edges_by_target from_node
            |> Enum.fold
              (fun (work_list, expand_set) (from_node', element) ->
                 Dph.perform_untargeted_dynamic_pop element action
                 |> Enum.fold
                   (fun (work_list, expand_set) (stack_action_list, to_state) ->
                      let to_node = State_node(to_state) in
                      let edge =
                        next_edge_in_sequence
                          from_node' stack_action_list to_node
                      in
                      let work = Work.Introduce_edge edge in
                      (work::work_list, expand_add to_node expand_set)
                   ) (work_list, expand_set)
              ) ([],Node_set.empty)
          in
          let expand_work = nodes_to_expand
                            |> Node_set.enum
                            |> Enum.map (fun node -> Work.Expand_node node)
          in
          add_works (Enum.append (List.enum work_list) expand_work) analysis
        in
        { analysis' with
          reachability = analysis'.reachability
                         |> Structure.add_untargeted_dynamic_pop_action
                           from_node action
        }
  ;;

  let rec fully_close analysis =
    if is_closed analysis
    then analysis
    else fully_close @@ closure_step analysis
  ;;

  let get_reachable_states state stack_actions analysis =
    lazy_logger `debug (fun () ->
        let (nodes,edges) = get_size analysis in
        Printf.sprintf "get_reachable_states: analysis has %d nodes and %d edges"
          nodes edges
      );
    let node = Intermediate_node(State_node(state), stack_actions) in
    if Node_set.mem node analysis.start_nodes
    then
      (*
        If a state is reachable by empty stack from the given starting state,
        then there will be a nop edge to it.  It's that simple once closure
        is finished.
      *)
      analysis.reachability
      |> Structure.find_nop_edges_by_source node
      |> Enum.filter_map
        (fun node ->
           match node with
           | State_node state -> Some state
           | Intermediate_node _ -> None
        )
    else
      raise @@ Reachability_request_for_non_start_state state
  ;;

  let dump_yojson analysis =
    `Assoc
      [ ( "node_awareness_map"
        , Node_map.to_yojson
            node_awareness_to_yojson
            analysis.node_awareness_map
        )
      ; ( "known_states"
        , State_set.to_yojson analysis.known_states
        )
      ; ( "start_nodes"
        , Node_set.to_yojson analysis.start_nodes
        )
      ; ( "reachability"
        , Structure.to_yojson analysis.reachability
        )
      ; ( "edge_function_count"
        , `Int (List.length analysis.edge_functions)
        )
      ; ( "untargeted_dynamic_pop_action_function_count"
        , `Int (List.length analysis.untargeted_dynamic_pop_action_functions)
        )
      ; ( "work_collection"
        , Work_collection_impl.to_yojson analysis.work_collection
        )
      ]
  ;;
end;;
