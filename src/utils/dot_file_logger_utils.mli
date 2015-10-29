(**
  This logger provides a functor for generating utility modules for logging
  [.dot] files.
*)

include module type of Dot_file_logger_utils_types;;

module Make :
  functor (Base : Dot_file_logger_base) ->
    Dot_file_logger_sig
      with type level = Base.level
       and type name = Base.name
       and type data = Base.data
       and type dot_node_id = Base.dot_node_id
;;
