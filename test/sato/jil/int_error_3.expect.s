((found_at_clause "ret = x and y") (number_of_errors 2)
 (error_list
  ((Match_error
    ((m_value ((x zero) 0)) (expected_type bool) (actual_type int)))
   (Match_error
    ((m_value ((y one) 1)) (expected_type bool) (actual_type int))))))