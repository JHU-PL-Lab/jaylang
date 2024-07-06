((found_at_clause "let (x : [`Int(int)|| `Bool(bool)]) = [{a = 1}] in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "[`Int(int)|| `Bool(bool)]") (t_actual_type "[{a = 1}]"))))))