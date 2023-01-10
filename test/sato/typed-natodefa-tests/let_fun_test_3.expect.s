((found_at_clause
  "let f (g : (int -> int)) (x : ((int -> int) -> bool)) : int = x g in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f)
     (t_expected_type "((int -> int) -> (((int -> int) -> bool) -> int))")
     (t_actual_type "((int -> int) -> (((int -> int) -> bool) -> bool))"))))))