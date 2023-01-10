((found_at_clause "let (x : {int | fun a -> (fun b -> 0 > b) a}) = 1 in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "{int | fun a -> (fun b -> 0 > b) a}")
     (t_actual_type "{int | Predicate Violated!}"))))))