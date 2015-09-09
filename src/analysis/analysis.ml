(**
  This module gives an implementation of the CBA analysis.  It is parametric
  in the choice of context stack.
*)

open Analysis_context_stack;;

let logger = Logger_utils.make_logger "Analysis";;

(**
  A functor which constructs a CBA analysis module.
*)
module Make(C : Context_stack) =
struct
  
end;;
