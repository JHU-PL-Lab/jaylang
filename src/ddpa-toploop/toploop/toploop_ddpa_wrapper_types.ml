(**
   A type utility declaration module.  This module is declared separately so
   its types need not be redeclared in an interface.
*)

open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ddpa_analysis_logging;;
open Ddpa_context_stack;;
open Ddpa_abstract_ast;;

module type DDPA_wrapper = sig
  type analysis

  module C : Context_stack;;

  val create_analysis :
    ?logging_config:(ddpa_analysis_logging_config option) -> expr -> analysis

  val values_of_variable_from :
    abstract_var -> annotated_clause -> analysis -> Abs_value_set.t

  val contextual_values_of_variable_from :
    abstract_var -> annotated_clause -> C.t -> analysis -> Abs_value_set.t

  val expression_of : analysis -> expr

  val pp_analysis : Format.formatter -> analysis -> unit
  val show_analysis : analysis -> string

  val get_size : analysis -> int * int * int * int * int
end;;
