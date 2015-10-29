(**
  A module to provide a global logging interface for CBA graphs.
*)

include module type of Cba_graph_logger_utils_types;;

module Make : functor (Basis : Cba_graph_logger_basis) -> Cba_graph_logger_sig;;
