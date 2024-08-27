((found_at_clause
  "let rec map (type t1 t2 ) (mf : (t1 -> t2)) (l : [t1]) : [t2] = match l with | [] -> [] | hd :: tl -> hd :: map t1 t2 mf tl end in map")
 (number_of_errors 1)
 (error_list
  ((Type_error
    ((t_var map) (t_expected_type "((t1 -> t2) -> ([t1] -> [t2]))")
     (t_actual_type "((t1 -> t2) -> ([t1] -> [t1]))"))))))