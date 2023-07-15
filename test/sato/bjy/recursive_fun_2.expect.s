((found_at_clause
  "let rec f (x : int) : bool = if x == 0 then 0 else f (x - 1) in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f) (t_expected_type "(int -> bool)")
     (t_actual_type "(int -> int)"))))))