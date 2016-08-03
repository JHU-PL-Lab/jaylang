(**
   This module contains a functor which generates a logging module for a
   particular PDS reachability tool.
*)

include module type of Pds_reachability_logger_utils_types;;

module Make :
  functor (Basis : Pds_reachability_basis.Basis)
    (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State
    )
    (Types : Pds_reachability_types.Types
     with module Stack_element = Basis.Stack_element
      and module State = Basis.State
      and module Targeted_dynamic_pop_action =
            Dph.Targeted_dynamic_pop_action
      and module Untargeted_dynamic_pop_action =
            Dph.Untargeted_dynamic_pop_action
    )
    (Structure : Pds_reachability_structure.Structure
     with module Stack_element = Basis.Stack_element
      and module Edge = Types.Edge
      and module Node = Types.Node
      and module Targeted_dynamic_pop_action =
            Types.Targeted_dynamic_pop_action
      and module Untargeted_dynamic_pop_action =
            Types.Untargeted_dynamic_pop_action
    ) ->
    Dot_file_logger_utils.Dot_file_logger_sig
  with type level = pds_reachability_logger_level
   and type name = pds_reachability_logger_name
   and type data = Structure.t
;;
