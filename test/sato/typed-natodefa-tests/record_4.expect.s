((found_at_clause
  "let test (test_record : {{:a : int, b : {:c : int:}:} | record_constraint}) : {bool | isTrue} = test_record.a + (test_record.b).c < 0 in test")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var test)
     (t_expected_type
      "({{:a : int, b : {:c : int:}:} | record_constraint} -> {bool | isTrue})")
     (t_actual_type
      "({{:a : int, b : {:c : int:}:} | record_constraint} -> {bool | Predicate Violated!})"))))))