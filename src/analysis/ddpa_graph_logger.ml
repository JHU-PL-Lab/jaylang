(**
   A logger module for DDPA graphs.
*)

open Batteries;;

open Ast;;
open Ast_pp;;
open Ddpa_graph;;
open Dot_file_logger_utils;;

include Ddpa_graph_logger_types;;

module Dot_file_basis =
struct
  type level = ddpa_graph_logger_level;;
  let compare_level = compare_ddpa_graph_logger_level;;
  let pp_level = pp_ddpa_graph_logger_level;;
  let default_level = Ddpa_log_none;;

  type name = ddpa_graph_name
  type data = ddpa_graph;;

  type dot_node_id = annotated_clause;;
  let string_of_dot_node_id acl =
    match acl with
    | Unannotated_clause(Abs_clause(Var(i,_),_)) -> show_ident i
    | Enter_clause(Var(i1,_),Var(i2,_),(Abs_clause(Var(i,_),_))) ->
      Printf.sprintf "%s=%s@%s<"
        (show_ident i1) (show_ident i2) (show_ident i)
    | Exit_clause(Var(i1,_),Var(i2,_),(Abs_clause(Var(i,_),_))) ->
      Printf.sprintf "%s=%s@%s>"
        (show_ident i1) (show_ident i2) (show_ident i)
    | Start_clause -> "start"
    | End_clause -> "end"
  ;;

  let graph_of ddpa_graph =
    let immediate_node_color = "#44ff44" in
    let non_immediate_node_color = "gray" in
    let wiring_node_color = "#ff8844" in
    let nodes =
      Ddpa_graph.edges_of ddpa_graph
      |> Enum.fold
        (fun acls (Ddpa_edge(acl,acl')) ->
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
      Ddpa_graph.edges_of ddpa_graph
      |> Enum.map
        (fun (Ddpa_edge(acl0,acl1)) ->
           { dot_edge_source = acl0
           ; dot_edge_target = acl1
           ; dot_edge_text = None
           })
    in
    (nodes,edges)
  ;;
  let string_of_name name =
    let (prefix,suffix) =
      match name with
      | Ddpa_graph_name_initial pfx -> (pfx,"initial")
      | Ddpa_graph_name_closed pfx -> (pfx,"final")
      | Ddpa_graph_name_intermediate(pfx,n) -> (pfx,string_of_int n)
    in prefix ^ "_DDPA_" ^ suffix
  ;;
end;;

module Logger = Dot_file_logger_utils.Make(Dot_file_basis);;

include Logger;;
