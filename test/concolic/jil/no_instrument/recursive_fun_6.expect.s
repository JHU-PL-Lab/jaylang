((found_at_clause
  "let rec sum (acc : int) (x : int) : bool = if x == 0 then acc else let acc2 = x + acc in sum (x - 1) acc2 in let sum2 = sum 0 in sum2")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var sum) (t_expected_type "(int -> (int -> bool))")
     (t_actual_type "(int -> (int -> int))"))))))