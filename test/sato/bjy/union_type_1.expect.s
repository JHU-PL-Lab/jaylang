((found_at_clause "let (x : {:a : bool:} v int) = true in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "{:a : bool:} v int") (t_actual_type bool))))))