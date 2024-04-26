((found_at_clause
  "let g (b : B a) : int = if a > 0 then 1 else if b then true else 2 in g")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var g) (t_expected_type "((b : B a) -> int)")
     (t_actual_type "((b : B a) -> bool)"))))))