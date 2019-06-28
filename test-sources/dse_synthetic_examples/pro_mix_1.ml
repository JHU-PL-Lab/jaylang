let input = 42
let failure = 42

let work w =
  let work_start = 0
  and work_end = w
  and work_j_start = 0
  in let rec work_loop work_j work_i =
    if work_i = work_end
    then 0
    else (
      work_loop (work_j + 1) (work_i + 1)
    )
  in (work_loop work_j_start work_start); 0

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
    if (f_sum_result = 0) && (f_m = 43)
    then assert false
    else 0
  in f_while true

let g m n =
  let g_start = 0
  and g_end = 1000
  in let rec g_loop g_i =
    if g_i = g_end
    then 0
    else (
      if m = g_i
      then (f m n; 0)
      else 0;
      g_loop (g_i + 1)
    )
  in g_loop g_start

let main_start = 0
let main_end = 1000

;;
let m = input
and n = input
in 
  work 3;
  if m >= 30
  then g m n
  else 0