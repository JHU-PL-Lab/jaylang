((found_at_clause
  "let rec fold (acc : int)  (foldf : (int -> (int -> bool))) (lst : [int]) : int = match lst with | [] -> acc | hd :: tl -> fold (foldf acc hd) foldf tl end in fold")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var fold)
     (t_expected_type "(int -> ((int -> (int -> bool)) -> ([int] -> int)))")
     (t_actual_type "(int -> ((int -> (int -> bool)) -> ([int] -> bool)))"))))))