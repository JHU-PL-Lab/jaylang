((found_at_clause
  "let f (x : [int]) : {int | fun a -> a == sum x 0} = sum x 1 in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f)
     (t_expected_type "((x : [int]) -> {int | fun a -> a == sum x 0})")
     (t_actual_type "((x : [int]) -> {int | Predicate Violated!})"))))))