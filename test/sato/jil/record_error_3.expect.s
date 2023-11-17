((found_at_clause "s = r ? ( s1 = 3 ) : ( s2 = 4 )") (number_of_errors 1)
 (error_list
  ((Match_error
    ((m_value ((r) "{one = o, zero = z}")) (expected_type bool)
     (actual_type "{one, zero}"))))))