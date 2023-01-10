((found_at_clause
  "let test (f : (int -> int) ^ (bool -> bool)) : int = f true in test")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var test) (t_expected_type "((int -> int) ^ (bool -> bool) -> int)")
     (t_actual_type "((int -> int) ^ (bool -> bool) -> bool)"))))))