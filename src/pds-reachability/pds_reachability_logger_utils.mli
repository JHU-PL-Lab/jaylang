(**
  This module contains a functor which generates a logging module for a
  particular PDS reachability tool.
*)

include module type of Pds_reachability_logger_utils_types;;

module Make :
  functor (Basis : Pds_reachability_basis.Basis)
          (Dph : Pds_reachability_types_stack.Dynamic_pop_handler
              with type stack_element = Basis.stack_element
               and type state = Basis.state
          )
          (Types : Pds_reachability_types.Types
              with type stack_element = Basis.stack_element
               and type state = Basis.state
               and type targeted_dynamic_pop_action =
                          Dph.targeted_dynamic_pop_action
               and type untargeted_dynamic_pop_action =
                          Dph.untargeted_dynamic_pop_action
          )
          (Structure : Pds_reachability_structure.Structure
              with type stack_element = Basis.stack_element
               and type edge = Types.edge
               and type node = Types.node
               and type targeted_dynamic_pop_action =
                          Types.targeted_dynamic_pop_action
               and type untargeted_dynamic_pop_action =
                          Types.untargeted_dynamic_pop_action
          ) ->    
  Dot_file_logger_utils.Dot_file_logger_sig
    with type level = pds_reachability_logger_level
     and type name = pds_reachability_logger_name
     and type data = Structure.structure
;;