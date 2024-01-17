((found_at_clause
  "let (x : {:a : bool, b : {:a : bool, b : int:} v int:}) = {a = true, b = true } in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x)
     (t_expected_type "{:a : bool, b : {:a : bool, b : int:} v int:}")
     (t_actual_type "{:a : bool, b : bool:}"))))))