(**
  A logger module for CBA graphs.
*)

open Batteries;;

open Ast;;
open Ast_pretty;;
open Cba_graph;;
open Dot_file_logger_utils;;

include Cba_graph_logger_types;;

let name_prefix = ref None;;

module Basis =
struct
  type level = cba_graph_logger_level;;
  let compare_level = compare_cba_graph_logger_level;;
  let pp_level = pp_cba_graph_logger_level;;
  let default_level = Cba_log_none;;

  type action = cba_graph_logger_action;;
  let level_of action =
    match action with
    | Cba_log_initial_graph _ -> Cba_log_all
    | Cba_log_closed_graph _ -> Cba_log_result
    | Cba_log_intermediate_graph _ -> Cba_log_all
  ;;

  type dot_node_id = annotated_clause;;
  let string_of_dot_node_id acl =
    match acl with
    | Unannotated_clause(Abs_clause(Var(i,_),_)) -> pretty_ident i
    | Enter_clause(Var(i1,_),Var(i2,_),(Abs_clause(Var(i,_),_))) ->
      Printf.sprintf "%s=%s@%s<"
        (pretty_ident i1) (pretty_ident i2) (pretty_ident i)
    | Exit_clause(Var(i1,_),Var(i2,_),(Abs_clause(Var(i,_),_))) ->
      Printf.sprintf "%s=%s@%s>"
        (pretty_ident i1) (pretty_ident i2) (pretty_ident i)
    | Start_clause -> "start"
    | End_clause -> "end"
  ;;

  let graph_of action =
    let cba_graph =
      match action with
      | Cba_log_initial_graph g -> g
      | Cba_log_closed_graph g -> g
      | Cba_log_intermediate_graph(g,_) -> g
    in
    let immediate_node_color = "#44ff44" in
    let non_immediate_node_color = "gray" in
    let wiring_node_color = "ff8844" in
    let nodes =
      Cba_graph.edges_of cba_graph
      |> Enum.fold
        (fun acls (Cba_edge(acl,acl')) ->
          Annotated_clause_set.add acl @@ Annotated_clause_set.add acl' acls)
        Annotated_clause_set.empty
      |> Annotated_clause_set.enum
      |> Enum.map
        (fun acl ->
          { dot_node_id = acl
          ; dot_node_color =
            begin
              let color =
                match acl with
                | Unannotated_clause(abs_cl) ->
                  begin
                    if is_abstract_clause_immediate abs_cl
                    then immediate_node_color
                    else non_immediate_node_color
                  end
                | Enter_clause _ | Exit_clause _ -> wiring_node_color
                | Start_clause | End_clause -> immediate_node_color
              in
              Some color
            end
          ; dot_node_text = Some(string_of_dot_node_id acl)
          })
    in
    let edges =
      Cba_graph.edges_of cba_graph
      |> Enum.map
        (fun (Cba_edge(acl0,acl1)) ->
          { dot_edge_source = acl0
          ; dot_edge_target = acl1
          ; dot_edge_text = None
          })
    in
    (nodes,edges)
  ;;
  let name_of action =
    match !name_prefix with
    | None -> raise @@
      Utils.Invariant_failure "Must set name prefix of CBA graph logger before logging a graph"
    | Some prefix ->
      prefix ^ "_CBA_" ^
      match action with
      | Cba_log_initial_graph _ -> "initial"
      | Cba_log_closed_graph _ -> "final"
      | Cba_log_intermediate_graph(_,n) -> string_of_int n
  ;;
end;;

module Logger = Dot_file_logger_utils.Make(Basis);;

include Logger;;

let set_name_prefix name = name_prefix := Some name;;
