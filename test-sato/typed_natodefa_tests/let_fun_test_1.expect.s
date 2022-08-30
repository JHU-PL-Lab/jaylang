((found_at_clause "let (id : (bool -> bool)) x = 1 + 1 in id")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var id) (t_expected_type "(bool -> bool)")
     (t_actual_type "(bool -> int)"))))))