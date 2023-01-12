((found_at_clause "let (x : [int v bool]) = [{a = 1}] in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "[int v bool]") (t_actual_type "[{a = 1}]"))))))