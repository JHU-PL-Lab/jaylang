module Int_map : BatMap.S with type key = int;;

type source_statistics =
  { ss_num_program_points : int;
    ss_num_function_definitions : int;
    ss_num_function_calls : int;
    ss_num_variable_references : int;
    ss_num_non_local_variable_references : int;
    ss_num_non_local_variable_references_by_depth : int Int_map.t;
    ss_max_lexical_depth : int;
  }
;;

val calculate_statistics : Ast.expr -> source_statistics;;
