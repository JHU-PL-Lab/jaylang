((found_at_clause
  "let negation (x : {int | isNeg}) : {int | isPos} = (0 - 1) * x in negation")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var negation) (t_expected_type "({int | isNeg} -> {int | isPos})")
     (t_actual_type "({int | isNeg} -> {int | TypeError: Predicate Violated!})"))))))