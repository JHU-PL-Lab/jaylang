(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Cba_graph_logger module interface and implementation; it should
  not be used directly.
*)

open Cba_graph;;

type cba_graph_logger_action =
  | Cba_log_initial_graph of cba_graph
  | Cba_log_closed_graph of cba_graph
  | Cba_log_intermediate_graph of cba_graph * int
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
