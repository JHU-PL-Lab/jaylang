((found_at_clause "let id (x : int) : int = x > 0 in id")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var id) (t_expected_type "(int -> int)")
     (t_actual_type "(int -> bool)"))))))