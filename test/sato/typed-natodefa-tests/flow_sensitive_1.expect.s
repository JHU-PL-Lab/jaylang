((found_at_clause
  "let g (x : int) : {int | fun a -> a < 0} = if x > 0 then f x else f 0 in g")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var g) (t_expected_type "(int -> {int | fun a -> a < 0})")
     (t_actual_type "(int -> {int | Predicate Violated!})"))))))