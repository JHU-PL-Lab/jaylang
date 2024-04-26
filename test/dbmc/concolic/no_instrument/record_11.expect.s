((found_at_clause
  "match r.~decl_lbls with | {b = x} -> let x = actual_rec_b_13.b in x end")
 (number_of_errors 1)
 (error_list
  ((Match_error
    ((m_value (() "{a = {}}")) (expected_type "Record {b}")
     (actual_type "Record {a}"))))))