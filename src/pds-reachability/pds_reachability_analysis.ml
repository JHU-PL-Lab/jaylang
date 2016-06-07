(**
   This module defines the actual PDS reachability analysis.
*)

open Batteries;;

open Pds_reachability_logger_utils;;
open Pds_reachability_types_stack;;
open Pp_utils;;

let lazy_logger = Logger_utils.make_lazy_logger "Pds_reachability_analysis";;

module type Analysis =
sig
  include Pds_reachability_types.Types;;

  (** The type of edge-generating functions used in this analysis. *)
  type edge_function = state -> (stack_action list * state) Enum.t

  (** The type of functions to generate untargeted dynamic pop actions in this
      analysis. *)
  type untargeted_dynamic_pop_action_function =
    state -> untargeted_dynamic_pop_action Enum.t

  exception Reachability_request_for_non_start_state of state;;

  (** The type of a reachability analysis in this module. *)
  type analysis

  (** The empty analysis.  This analysis has no states, edges, or edge
      functions. *)
  val empty : ?logging_prefix:string option -> unit -> analysis

  (** Adds a single edge to a reachability analysis. *)
  val add_edge
    : state -> stack_action list -> state -> analysis -> analysis

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
    : state -> untargeted_dynamic_pop_action -> analysis -> analysis

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
    : state
    -> stack_action list
    -> analysis
    -> analysis

  (** Determines the states which are reachable from a given state and initial
      stack element.  This state must have been added to the analysis
      previously. *)
  val get_reachable_states
    : state
    -> stack_action list
    -> analysis
    -> state Enum.t

  (** Pretty-printing function for the analysis. *)
  val pp_analysis : analysis pretty_printer
  val show_analysis : analysis -> string

  (** An exception raised when a reachable state query occurs before the state
      is added as a start state. *)
  exception Reachability_request_for_non_start_state of state;;

  (** Configures the logging level for the analyses produced by this module.
      This affects all such analyses, not just future analyses. *)
  val set_logging_level : pds_reachability_logger_level -> unit

  (** Retrieves the logging level used by analyses produced by this module. *)
  val get_logging_level : unit -> pds_reachability_logger_level

  (** Determines the size of the provided analysis in terms of both node and
      edge count (respectively). *)
  val get_size : analysis -> int * int
end;;

module Make
    (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with type stack_element = Basis.stack_element
      and type state = Basis.state)
    (Work_collection_template_impl :
       Pds_reachability_work_collection.Work_collection_template)
  : Analysis
    with type state = Basis.state
     and type stack_element = Basis.stack_element
     and type targeted_dynamic_pop_action = Dph.targeted_dynamic_pop_action
     and type untargeted_dynamic_pop_action = Dph.untargeted_dynamic_pop_action
=
struct
  (********** Create and wire in appropriate components. **********)

  module Types = Pds_reachability_types.Make(Basis)(Dph);;
  module Work = Pds_reachability_work.Make(Basis)(Types);;
  module Work_collection_impl = Work_collection_template_impl(Work);;
  module Structure = Pds_reachability_structure.Make(Basis)(Dph)(Types);;
  module Logger =
    Pds_reachability_logger_utils.Make(Basis)(Dph)(Types)(Structure)
  ;;

  include Types;;

  type edge_function = state -> (stack_action list * state) Enum.t;;
  type untargeted_dynamic_pop_action_function =
    state -> untargeted_dynamic_pop_action Enum.t;;

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

  type analysis_logging_data =
    { analysis_logging_prefix : string
    ; major_log_index : int
    ; minor_log_index : int
    }
    [@@deriving show]
  ;;
  let _show_analysis_logging_data = show_analysis_logging_data;;

  type analysis =
    { known_nodes : Node_set.t
          [@printer fun formatter nodes ->
                 pp_concat_sep_delim "{" "}" "," pp_node formatter @@
                 Node_set.enum nodes]
    ; reachability : Structure.structure
    ; edge_functions : edge_function list
          [@printer fun formatter functions ->
                 Format.fprintf formatter "(length = %d)"
                   (List.length functions)]
    ; untargeted_dynamic_pop_action_functions :
        untargeted_dynamic_pop_action_function list
          [@printer fun formatter functions ->
                 Format.fprintf formatter "(length = %d)"
                   (List.length functions)]
    ; work_collection : Work_collection_impl.work_collection
    ; logging_data : analysis_logging_data option
    }
    [@@deriving show]
  ;;

  (********** Define analysis operations. **********)

  let log_analysis level analysis =
    match analysis.logging_data with
    | None -> ()
    | Some data ->
      let name = Pds_reachability_logger_name(
          data.analysis_logging_prefix,
          data.major_log_index,
          data.minor_log_index)
      in
      Logger.log level name analysis.reachability
  ;;

  let log_step inc_fn level analysis =
    match analysis.logging_data with
    | None -> analysis
    | Some data ->
      log_analysis level analysis;
      lazy_logger `trace
        (fun () ->
           let name = Pds_reachability_logger_name(
               data.analysis_logging_prefix,
               data.major_log_index,
               data.minor_log_index)
           in
           Printf.sprintf "Logging graph %s" @@ Logger.string_of_name name
        );
      let data' = inc_fn data in
      { analysis with logging_data = Some data' }
  ;;

  let log_major =
    log_step (fun data ->
        { data with
          major_log_index = data.major_log_index + 1; minor_log_index = 0 })
  ;;

  let log_minor =
    log_step (fun data ->
        { data with minor_log_index = data.minor_log_index + 1 })
  ;;

  let get_logging_level () = Logger.get_level ();;

  let set_logging_level level = Logger.set_level level;;

  let get_size analysis =
    let reachability = analysis.reachability in
    let node_count = Enum.count @@ Structure.enumerate_nodes reachability in
    let edge_count = Enum.count @@ Structure.enumerate_edges reachability in
    (node_count, edge_count)
  ;;

  let empty ?logging_prefix:(logging_prefix=None) () =
    { known_nodes = Node_set.empty
    ; reachability = Structure.empty
    ; edge_functions = []
    ; untargeted_dynamic_pop_action_functions = []
    ; logging_data =
        begin
          match logging_prefix with
          | None -> None
          | Some pfx -> Some
                          { analysis_logging_prefix = pfx
                          ; major_log_index = 0
                          ; minor_log_index = 0
                          }
        end
    ; work_collection = Work_collection_impl.empty
    };;

  (** Adds a "real" edge (the PDS reachability structure's edge, not the
      analysis interface's presentation of an edge) to an analysis.  Then,
      performs edge closure on the analysis. *)
  let rec add_real_edge_and_close edge analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () -> "add_real_edge_and_close " ^ show_edge edge)
      (fun _ -> "Finished") @@
    fun () ->
    (* If we already have this edge, ignore the addition.  In particular, we can
       get an infinite loop (resulting in stack overflow) if we allow the
       closure to proceed. *)
    if Structure.has_edge edge analysis.reachability then analysis else
      log_minor Pds_reachability_log_each_edge @@
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
        | Pop _ | Pop_dynamic_targeted _ -> analysis'
      in
      (* Next, we need to perform edge closure.  The particular action depends
         on this new edge's action.  The closure rules are summarized below.
          1. (a) -- push a --> (b) -- pop a --> (c) ==> (a) -- nop --> (c)
          2. (a) -- push a --> (b) -- nop --> (c) ==> (a) -- push a --> (c)
          3. (a) -- nop --> (b) -- nop --> (c) ==> (a) -- nop --> (c)
          4. (a) -- push a --> (b) -- targeted_dynamic_pop f --> (c) ==>
              (a) -- action_1 --> ... -- action_n --> (c)
             for each [action_1,...,action_n] in f a
          5. (a) -- push a --> (b) -- untargeted_dynamic_pop f ==>
              (a) -- action_1 --> ... -- action_n --> (c)
             for each ([action_1,...,action_n],c) in f a
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
        | Pop_dynamic_targeted _ -> Enum.empty ()
      in
      (* analysis_updates is an enumeration of functions which will update the
         analysis by adding edges.  This is appropriate when e.g. new
         intermediate nodes must be created. *)
      let analysis_updates =
        match edge.edge_action with
        | Nop -> Enum.empty ()
        | Push element ->
          let targeted_dynamic_pop_out_of_target =
            analysis''.reachability
            |> Structure.find_targeted_dynamic_pop_edges_by_source edge.target
            |> Enum.map
              (fun (target', action) ->
                 fun (analysis : analysis) ->
                   Dph.perform_targeted_dynamic_pop element action
                   |> Enum.fold
                     (fun analysis' actions ->
                        add_edges_between_nodes
                          edge.source target' actions analysis')
                     analysis
              )
          in
          let untargeted_dynamic_pop_out_of_target =
            analysis''.reachability
            |> Structure.find_untargeted_dynamic_pop_actions_by_source
              edge.target
            |> Enum.map (Dph.perform_untargeted_dynamic_pop element)
            |> Enum.concat
            |> Enum.map
              (fun (path, target) ->
                 let target_node = State_node target in
                 add_edges_between_nodes edge.source target_node path)
          in
          Enum.append
            untargeted_dynamic_pop_out_of_target
            targeted_dynamic_pop_out_of_target
        | Pop _ -> Enum.empty ()
        | Pop_dynamic_targeted action ->
          let push_into_source =
            analysis''.reachability
            |> Structure.find_push_edges_by_target edge.source
            |> Enum.map
              (fun (source, element) ->
                 fun analysis ->
                   Dph.perform_targeted_dynamic_pop element action
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
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun () ->
         Printf.sprintf "add_edges_between_nodes(%s,%s,%s)"
           (Types.show_node source_node) (Types.show_node target_node)
           (String_utils.string_of_list (Types.show_stack_action) stack_actions)
      )
      (fun _ -> "Finished") @@
    fun () ->
    (* This recursive function creates the chain of single stack action edges
       from the provided list of stack actions.  It consumes intermediate node
       numbers as necessary to generate the edges, so it takes and returns this
       value. *)
    let rec mk_real_edges actions next_source =
      match actions with
      | [] ->
        let edge =
          { source = next_source
          ; target = target_node
          ; edge_action = Nop
          }
        in
        [edge]
      | [action] ->
        let edge =
          { source = next_source
          ; target = target_node
          ; edge_action = action
          }
        in
        [edge]
      | action::actions' ->
        let target = Intermediate_node(target_node, actions') in
        let edge =
          { source = next_source
          ; target = target
          ; edge_action = action
          }
        in
        let real_edges = mk_real_edges actions' target in
        edge :: real_edges
    in
    (* Let's figure out which new single-action edges we will be adding. *)
    let real_edges = mk_real_edges stack_actions source_node in
    let analysis' =
      add_node source_node @@ add_node target_node analysis
    in
    (* Log the result. *)
    lazy_logger `trace @@
    (fun () ->
       Printf.sprintf "In add_edges_between_nodes(%s,%s,%s), generated edges: %s"
         (Types.show_node source_node) (Types.show_node target_node)
         (String_utils.string_of_list (Types.show_stack_action) stack_actions)
         (String_utils.string_of_list Types.show_edge real_edges)
    );
    (* Now, let's add them. *)
    real_edges
    |> List.fold_left
      (fun analysis'' real_edge ->
         add_real_edge_and_close real_edge analysis'')
      analysis'

  (**
     Adds an edge to this analysis.  Here, "edge" refers to the interface's
     presentation of an edge structure.
  *)
  and add_edge source_state stack_action_list target_state analysis =
    let source_node = State_node(source_state) in
    let target_node = State_node(target_state) in
    let analysis' =
      add_edges_between_nodes source_node target_node stack_action_list analysis
    in
    log_major Pds_reachability_log_each_call analysis'

  and add_untargeted_dynamic_pop_action source_state action analysis =
    let source_node = State_node source_state in
    if Structure.has_untargeted_dynamic_pop_action source_node action
        analysis.reachability
    then analysis
    else
      let analysis' =
        { analysis with
          reachability =
            Structure.add_untargeted_dynamic_pop_action source_node action
              analysis.reachability
        }
      in
      (* Any existing pushes into the source of this pop action may generate
         new edges.  Collect and add them. *)
      analysis'.reachability
      |> Structure.find_push_edges_by_target source_node
      |> Enum.map
        (fun (push_source_node, element) ->
           Dph.perform_untargeted_dynamic_pop element action
           |> Enum.map (fun x -> (push_source_node,x))
        )
      |> Enum.concat
      |> Enum.fold
        (fun analysis'' (push_source_node, (path, target_state)) ->
           let target_node = State_node target_state in
           add_edges_between_nodes push_source_node target_node path analysis'')
        analysis'

  (**
     Adds a node to the analysis.  Specifically, adds all edges with that node
     as a source that are returned by the analysis's edge functions.
  *)
  and add_node node analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ ->
         "add_node (" ^ show_node node ^ ")")
      (fun _ -> "Finished") @@
    fun () ->
    if Node_set.mem node analysis.known_nodes then analysis else
      let analysis' =
        { analysis with
          known_nodes = Node_set.add node analysis.known_nodes }
      in
      match node with
      | Intermediate_node _ -> analysis'
      | State_node state ->
        let analysis'' =
          analysis.edge_functions
          |> List.enum
          |> Enum.map (fun f -> f state)
          |> Enum.concat
          |> Enum.fold
            (fun analysis_so_far (actions,target_state) ->
               add_edge state actions target_state analysis_so_far)
            analysis'
        in
        let analysis''' =
          analysis.untargeted_dynamic_pop_action_functions
          |> List.enum
          |> Enum.map (fun f -> f state)
          |> Enum.concat
          |> Enum.fold
            (fun analysis_so_far action ->
               add_untargeted_dynamic_pop_action state action analysis_so_far)
            analysis''
        in
        analysis'''
  ;;

  let add_edge_function edge_function analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ -> "add_edge_function (...)")
      (fun _ -> "Finished") @@
    fun () ->
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
         | Intermediate_node _ -> None)
    |> Enum.map
      (fun source_state ->
         edge_function source_state |> Enum.map (fun x -> (source_state, x)))
    |> Enum.concat
    |> Enum.fold
      (fun analysis'' (source_state,(action_list,target_state)) ->
         add_edge source_state action_list target_state analysis'')
      analysis'
    |> log_major Pds_reachability_log_each_call
  ;;

  let add_untargeted_dynamic_pop_action_function fn analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ -> "add_untargeted_dynamic_pop_action_function(...)")
      (fun _ -> "Finished") @@
    fun () ->
    let analysis' =
      { analysis with
        untargeted_dynamic_pop_action_functions =
          fn::analysis.untargeted_dynamic_pop_action_functions
      }
    in
    (* There may be nodes already in the graph which need the pop actions from
       this function.  Find them and add those actions. *)
    analysis'.known_nodes
    |> Node_set.enum
    |> Enum.filter_map
      (fun node ->
         match node with
         | State_node state -> Some state
         | Intermediate_node _ -> None)
    |> Enum.map
      (fun source_state ->
         fn source_state |> Enum.map (fun x -> (source_state, x)))
    |> Enum.concat
    |> Enum.fold
      (fun analysis'' (source_state, action) ->
         add_untargeted_dynamic_pop_action source_state action analysis'')
      analysis'
    |> log_major Pds_reachability_log_each_call
  ;;

  let add_start_state state stack_actions analysis =
    Logger_utils.lazy_bracket_log (lazy_logger `trace)
      (fun _ -> Printf.sprintf "add_start_state(%s,...)"
          (pp_to_string Basis.pp_state state))
      (fun _ -> "Finished") @@
    fun () ->
    analysis
    |> add_edges_between_nodes
      (Intermediate_node(State_node(state), stack_actions))
      (State_node(state))
      stack_actions
    |> log_major Pds_reachability_log_each_call
  ;;

  let get_reachable_states state stack_actions analysis =
    let node = Intermediate_node(State_node(state), stack_actions) in
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
        )
    else
      raise @@ Reachability_request_for_non_start_state state
  ;;
end;;
