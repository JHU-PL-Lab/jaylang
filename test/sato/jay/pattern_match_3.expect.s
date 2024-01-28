((found_at_clause "match a with | int -> 0 | bool -> 1 | fun -> 2 end")
 (number_of_errors 3)
 (error_list
  ((Match_error
    ((m_value ((a) {})) (expected_type Integer) (actual_type "Record {}")))
   (Match_error
    ((m_value ((a) {})) (expected_type Boolean) (actual_type "Record {}")))
   (Match_error
    ((m_value ((a) {})) (expected_type Function) (actual_type "Record {}"))))))