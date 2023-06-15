((found_at_clause
  "let f (a : int) : (B a -> int) = let g (b : B a) : int = if a > 0 then 1 else if b then true else 2 in g in f")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var f) (t_expected_type "((a : int) -> (B a -> int))")
     (t_actual_type "((a : int) -> (B a -> bool))"))))))