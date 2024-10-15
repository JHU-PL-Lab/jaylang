((found_at_clause
  "let access_record (r : {:a : int, b : bool:}) : int = r.b in access_record")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var access_record)
     (t_expected_type "({:a : int, b : bool:} -> int)")
     (t_actual_type
      "({:a : int, b : bool:} -> bool)"))))))