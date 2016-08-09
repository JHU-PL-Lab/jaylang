(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Core_ast;;
open Ddpa_analysis_logging;;
open Ddpa_context_stack;;
open Ddpa_graph;;

module type DDPA_wrapper = sig
  type analysis

  module C : Context_stack;;

  val create_analysis :
    ?logging_config:(ddpa_analysis_logging_config option) -> expr -> analysis

  val values_of_variable_from :
    var -> annotated_clause -> analysis -> Abs_filtered_value_set.t

  val contextual_values_of_variable_from :
    var -> annotated_clause -> C.t -> analysis -> Abs_filtered_value_set.t

  val expression_of : analysis -> expr

  val pp_analysis : Format.formatter -> analysis -> unit
  val show_analysis : analysis -> string

  val get_size : analysis -> int * int * int * int * int
end;;
