((found_at_clause "let fst (type t1 t2) (x : t1) (y : t2) : t1 = y in fst")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var fst) (t_expected_type "(t1 -> (t2 -> t1))")
     (t_actual_type "(t1 -> (t2 -> t2))"))))))