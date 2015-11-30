(**
  A type-only declaration module.  This module is intended to be re-exported
  from the Toploop_cba module interface and implementation; it should not be
  used directly.
*)

open Batteries;;

open Ast;;
open Cba_graph;;

type inconsistency =
  | Application_of_non_function of var * clause * abstract_value
      (** Represents the application of a non-function value.  The arguments
          are the function variable, the call site clause, and the abstract
          non-function value which appeared at the call site. *);;

module type CBA = sig
  type analysis
  
  val create_analysis : ?logging_prefix:string option -> expr -> analysis
  
  val values_of_variable_from :
    var -> annotated_clause -> analysis -> Abs_value_set.t 
  
  val check_inconsistencies : analysis -> inconsistency Enum.t
  
  val pp_analysis : analysis -> string
end;;
