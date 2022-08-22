((found_at_clause "match `A 2 with | `B b -> b | `C c -> c end")
 (number_of_errors 2)
 (error_list
  ((Match_error
    ((m_value (() "`A 2")) (expected_type "Variant `B")
     (actual_type "Variant `A")))
   (Match_error
    ((m_value (() "`A 2")) (expected_type "Variant `C")
     (actual_type "Variant `A"))))))