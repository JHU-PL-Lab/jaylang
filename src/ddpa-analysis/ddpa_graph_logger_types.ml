(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Ddpa_graph_logger module interface and implementation; it should
  not be used directly.
*)

open Ddpa_graph;;

(** Describes the namespace of logged DDPA graphs.  Each name takes at least the
    prefix string naming the graph closure. *)
type ddpa_graph_name =
  | Ddpa_graph_name_initial of string
  | Ddpa_graph_name_closed of string
  | Ddpa_graph_name_intermediate of string * int
;;

type ddpa_graph_logger_level =
  | Ddpa_log_all
  | Ddpa_log_result
  | Ddpa_log_none
  [@@ deriving ord ]
;;

let pp_ddpa_graph_logger_level level =
  match level with
  | Ddpa_log_none -> "none"
  | Ddpa_log_result -> "result-only"
  | Ddpa_log_all -> "all"
;;

module type Ddpa_graph_logger_sig =
  Dot_file_logger_utils.Dot_file_logger_sig
      with type level = ddpa_graph_logger_level
       and type name = ddpa_graph_name
       and type data = ddpa_graph
       and type dot_node_id = annotated_clause
;;