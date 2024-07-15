((found_at_clause
  "let (check_type : module_type_check) = sample_module.module in sample_module")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var check_type)
     (t_expected_type "module_type_check")
     (t_actual_type "TypeError: Type unknown"))))))