((found_at_clause
  "let access_record (r : {{:a : int, b : bool:} | aIsPos}) : {int | isNeg} = r.a in access_record")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var access_record)
     (t_expected_type "({{:a : int, b : bool:} | aIsPos} -> {int | isNeg})")
     (t_actual_type
      "({{:a : int, b : bool:} | aIsPos} -> {int | TypeError: Predicate Violated!})"))))))