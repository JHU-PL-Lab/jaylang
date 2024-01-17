((found_at_clause
  "let list_test (lst : [int]) : int = match lst with | [] -> false | hd :: tl -> 5 end in list_test")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var list_test) (t_expected_type "([int] -> int)")
     (t_actual_type "([int] -> bool)"))))))