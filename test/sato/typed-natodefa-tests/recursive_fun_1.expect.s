((found_at_clause
  "let main (n : {int | fun a -> a >= 0}) : {int | fun a -> a > 0} = let l = makeList n in if n > 0 then getHead l else 0 in main")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var main)
     (t_expected_type "({int | fun a -> a >= 0} -> {int | fun a -> a > 0})")
     (t_actual_type
      "({int | fun a -> a >= 0} -> {int | Predicate Violated!})"))))))