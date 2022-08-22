((found_at_clause "match {} with | hd :: tl -> tl | [] -> [] end")
 (number_of_errors 2)
 (error_list
  ((Match_error
    ((m_value (() {})) (expected_type List) (actual_type "Record {}")))
   (Match_error
    ((m_value (() {})) (expected_type List) (actual_type "Record {}"))))))