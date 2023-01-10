((found_at_clause
  "let rec f (x : int) : int = if x == 0 then false else f (x - 1) > 0 in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f) (t_expected_type "(int -> int)")
     (t_actual_type "(int -> bool)"))))))