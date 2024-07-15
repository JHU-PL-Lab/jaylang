((found_at_clause "let g (y : int) : int = y >= 0 in g") (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var g) (t_expected_type "(int -> int)")
     (t_actual_type "(int -> bool)"))))))