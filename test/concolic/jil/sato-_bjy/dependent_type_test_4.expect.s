((found_at_clause
  "let f (l : {[int] | notNeg}) : {int | fun a -> a > 0 and sum l 0 == a} = sum l 0 in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f)
     (t_expected_type "((l : {[int] | notNeg}) -> {int | fun a -> a > 0 and sum l 0 == a})")
     (t_actual_type "((l : {[int] | notNeg}) -> {int | TypeError: Predicate Violated!})"))))))