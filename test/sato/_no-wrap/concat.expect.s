((found_at_clause
  "let rec prepend (l1 : [int]) (l2 : [bool]) : [int] = match l2 with | [] -> l1 | hd :: tl -> prepend (hd :: l1) tl end in prepend")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var prepend) (t_expected_type "([int] -> ([bool] -> [int]))")
     (t_actual_type "([int] -> ([bool] -> [bool]))"))))))