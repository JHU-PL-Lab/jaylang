((found_at_clause "let fst (x : 'a) (y : 'b) : 'a = y in fst")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var fst) (t_expected_type "('a -> ('b -> 'a))")
     (t_actual_type "('a -> ('b -> 'b))"))))))