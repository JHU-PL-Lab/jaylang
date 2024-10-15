((found_at_clause "let (x : {[int] | testFun}) = [val, val] in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "{[int] | testFun}")
     (t_actual_type "[val, val]"))))))