open Batteries;;

open Dot_file_logger_utils;;
open Pp_utils;;

include Pds_reachability_logger_utils_types;;

module Make(Basis : Pds_reachability_basis.Basis)
           (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
              with type stack_element = Basis.stack_element
               and type state = Basis.state
           )
           (Types : Pds_reachability_types.Types
              with type stack_element = Basis.stack_element
               and type state = Basis.state
               and type targeted_dynamic_pop_action =
                          Dph.targeted_dynamic_pop_action
               and type untargeted_dynamic_pop_action =
                          Dph.untargeted_dynamic_pop_action
           )
           (Structure : Pds_reachability_structure.Structure
              with type stack_element = Basis.stack_element
               and type edge = Types.edge
               and type node = Types.node
               and type targeted_dynamic_pop_action =
                          Types.targeted_dynamic_pop_action
               and type untargeted_dynamic_pop_action =
                          Types.untargeted_dynamic_pop_action
           ) =
struct
  module Logger_basis =
  struct
    type level = pds_reachability_logger_level;;
    let compare_level = compare_pds_reachability_logger_level;;
    let pp_level = pp_pds_reachability_logger_level;;
    let default_level = Pds_reachability_log_nothing;;

    type name = pds_reachability_logger_name;;
    let string_of_name (Pds_reachability_logger_name(pfx,major,minor)) =
      pfx ^ "_PDR_" ^ string_of_int major ^ "_" ^ string_of_int minor
    ;;

    type dot_node_id = Types.node;;
    let rec string_of_dot_node_id node =
      match node with
      | Types.State_node state -> pp_to_string Basis.pp_state state
      | Types.Intermediate_node (node',actions) ->
        Printf.sprintf "InterNode(%s,%s)"
          (string_of_dot_node_id node')
          (String_utils.string_of_list Types.show_stack_action actions)
    ;;

    type data = Structure.structure;;
    let string_of_edge_action = pp_to_string Types.pp_stack_action ;;
    let graph_of structure =
      let nodes = Structure.enumerate_nodes structure in
      let edges = Structure.enumerate_edges structure in
      let nodes' =
        nodes
        |> Enum.map
          (fun node ->
            { dot_node_id = node
            ; dot_node_color =
              begin
                match node with
                | Types.State_node _ -> Some "#aaccff"
                | Types.Intermediate_node _ -> Some "#ccaaff"
              end
            ; dot_node_text =
              Some (pp_to_string Types.pp_node node)
            }
          )
      in
      let edges' =
        edges
        |> Enum.map
          (fun edge ->
            { dot_edge_source = edge.Types.source
            ; dot_edge_target = edge.Types.target
            ; dot_edge_text =
              Some (string_of_edge_action edge.Types.edge_action)
            }
          )
      in
      (nodes',edges')
    ;;
  end;;

  module Logger = Dot_file_logger_utils.Make(Logger_basis);;

  include Logger;;
end;;
