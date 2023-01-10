((found_at_clause
  "let rec f (x : [int]) : ((y : [int]) -> ((z : [int]) -> {[int] | fun a -> length a 0 == length x 0 + length y 0})) = let g l2 = let h acc = match l2 with | [] -> [] | hd :: tl -> f l2 tl acc end in h in g in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f)
     (t_expected_type
      "((x : [int]) -> ((y : [int]) -> ((z : [int]) -> {[int] | fun a -> length a 0 == length x 0 + length y 0})))")
     (t_actual_type "((x : [int]) -> ((y : [int]) -> ((z : [int]) -> [])))"))))))