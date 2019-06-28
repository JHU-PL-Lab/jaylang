let input = 42
let failure = 42

let f f_m f_n = 
  let f_start = 0
  and f_end = 6
  and f_sum_init = 0
  in let rec f_loop f_loop_n f_sum f_i =
    if f_i = f_end
    then 0
    else (
      let f_a = f_n mod 2
      in let f_sum_next =  
            (if f_a <> 0
            then f_sum + (f_a + 1)
            else f_sum)
         and f_loop_n_next = f_loop_n / 2
         and f_i_next = f_i + 1
          in f_loop f_loop_n_next f_sum_next f_i_next
    )
  in let f_sum_result = f_loop f_n f_sum_init f_start
  in let rec f_while c =
    if (f_sum_result = 0) && (f_m = 7)
    then assert false
    else 0
  in f_while true

let main_start = 0
let main_end = 1000

;;
let m = 7
and n = input
in let rec main_loop main_i =
  if main_i = main_end 
  then 0 
  else (
    if m = main_i
    then (f m n; 0)
    else 0;
    main_loop (main_i+1)
  )
in main_loop main_start