(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Cba_graph_logger module interface and implementation; it should
  not be used directly.
*)

open Cba_graph;;

(** Describes the namespace of logged CBA graphs.  Each name takes at least the
    prefix string naming the graph closure. *)
type cba_graph_name =
  | Cba_graph_name_initial of string
  | Cba_graph_name_closed of string
  | Cba_graph_name_intermediate of string * int
;;

type cba_graph_logger_level =
  | Cba_log_all
  | Cba_log_result
  | Cba_log_none
  [@@ deriving ord ]
;;

let pp_cba_graph_logger_level level =
  match level with
  | Cba_log_none -> "none"
  | Cba_log_result -> "result-only"
  | Cba_log_all -> "all"
;;

module type Cba_graph_logger_sig =
  Dot_file_logger_utils.Dot_file_logger_sig
      with type level = cba_graph_logger_level
       and type name = cba_graph_name
       and type data = cba_graph
       and type dot_node_id = annotated_clause
;;