(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Cba_graph_logger module interface and implementation; it should
  not be used directly.
*)

open Cba_graph;;

(** Describes the logger actions for CBA graphs.  Each action takes at least a
    graph and the prefix string naming the graph. *)
type cba_graph_logger_action =
  | Cba_log_initial_graph of cba_graph * string
  | Cba_log_closed_graph of cba_graph * string
  | Cba_log_intermediate_graph of cba_graph * string * int
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
       and type action = cba_graph_logger_action
       and type dot_node_id = annotated_clause
;;