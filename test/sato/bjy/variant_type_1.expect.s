((found_at_clause "let (x : `A (int)) = `A true in x") (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "`A (int)") (t_actual_type "`A (bool)"))))))