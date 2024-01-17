((found_at_clause
  "let create_record (x : int) (y : bool) : {:a : int, b : bool:} = {a = x, b = x} in create_record")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var create_record)
     (t_expected_type "(int -> (bool -> {:a : int, b : bool:}))")
     (t_actual_type "(int -> (bool -> {:a : int, b : int:}))"))))))