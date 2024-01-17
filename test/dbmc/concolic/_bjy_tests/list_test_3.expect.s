((found_at_clause "let (lst : [int]) = [true] in lst") (number_of_errors 1)
 (error_list
  ((Type_error ((t_var lst) (t_expected_type [int]) (t_actual_type [bool]))))))