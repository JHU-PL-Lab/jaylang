((found_at_clause
  "let rec f (x : int) : bool = if x == 0 then 0 else g (x - 1) with g (y : int) : int = if y < 0 then false else f (y - 1) in f 1")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f) (t_expected_type "(int -> bool)")
     (t_actual_type "(int -> int)"))))))