(**
  A module to provide a global logging interface for CBA graphs.
*)

open Cba_graph;;
open Dot_file_logger_utils;;

include module type of Cba_graph_logger_types;;

include Dot_file_logger_sig
  with type level = cba_graph_logger_level
   and type action = cba_graph_logger_action
   and type dot_node_id = annotated_clause
;;

(** Sets the name prefix used by this logger module.  Must be callled prior to
    logging any DOT graphs. *)
val set_name_prefix : string -> unit;;
