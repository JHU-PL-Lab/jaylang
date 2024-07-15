((found_at_clause "let neg (x : int) : bool = (0 - 1) * x in neg")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var neg) (t_expected_type "(int -> bool)")
     (t_actual_type "(int -> int)"))))))