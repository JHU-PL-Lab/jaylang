((found_at_clause "let (x : bool ^ int) = true in x") (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "bool ^ int") (t_actual_type bool))))))