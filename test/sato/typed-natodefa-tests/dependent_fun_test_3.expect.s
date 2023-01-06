((found_at_clause
  "let main (n : int) : {int | fun c -> c < 0} = if n >= 0 then f n (h n) else 1 in main")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var main) (t_expected_type "(int -> {int | fun c -> c < 0})")
     (t_actual_type "(int -> {int | Predicate Violated!})"))))))