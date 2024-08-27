((found_at_clause
  "let test (f : (`Int(int) -> int) ^ (`Bool(bool) -> bool)) : int = f (`Bool true) in test")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var test) (t_expected_type "((`Int(int) -> int) ^ (`Bool(bool) -> bool) -> int)")
     (t_actual_type "((`Int(int) -> int) ^ (`Bool(bool) -> bool) -> bool)"))))))