((found_at_clause "let (x : `BRec({:a : bool:})|| `Int(int)) = true in x")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var x) (t_expected_type "`BRec({:a : bool:})|| `Int(int)") (t_actual_type bool))))))