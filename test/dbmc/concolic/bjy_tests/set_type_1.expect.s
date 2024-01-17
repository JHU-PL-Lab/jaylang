((found_at_clause
  "let inc (x : {int | isPos}) : {int | isNeg} = x + 1 in twice inc 0")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var inc) (t_expected_type "({int | isPos} -> {int | isNeg})")
     (t_actual_type "({int | isPos} -> {int | TypeError: Predicate Violated!})"))))))