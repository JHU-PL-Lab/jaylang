((found_at_clause
  "match {a = 1}.~decl_lbls with | {c = z} -> let z = actual_rec_b_1.c in 1 end")
 (number_of_errors 1)
 (error_list
  ((Match_error
    ((m_value (() "{a = {}}")) (expected_type "Record {c}")
     (actual_type "Record {a}"))))))