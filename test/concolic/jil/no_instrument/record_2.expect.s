((found_at_clause "let (r : {:a : int, b : bool:}) = {a = 1, b = 1} in r")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var r) (t_expected_type "{:a : int, b : bool:}")
     (t_actual_type "{:a : int, b : int:}"))))))