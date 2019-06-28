(* the original C code check how many args start with 'b',
    check in `*argv[i]`
 *)

let input = 42
let failure = 42

let argc = input
let i_start = 0
let n_start = 0
let arg_0 = 0
let arg_1 = 0
let arg_2 = 0
let arg_3 = 0
let arg_4 = 0
let arg_5 = 0
let arg_6 = 0
let arg_7 = 0

(* Char.code 'b' = 98 *)

let arg_index_at i =
  if i = 0 then arg_0
  else if i = 1 then arg_1
  else if i = 2 then arg_2
  else if i = 3 then arg_3
  else if i = 4 then arg_4
  else if i = 5 then arg_5
  else if i = 6 then arg_6
  else arg_7
;;

(* I feel like I will have to duplicate the conditions, or duplicate the innovation of next iteration. I prefer the former. *)
let rec loop n i = 
  if i = argc
  then 0
  else (
    let arg = arg_index_at i
    in 
      if arg = 98
      then (
        if n < 4
        then
          (* b[n++] = 1 is a mutable array *)
          0
        else
          failure
      )
      else (
        let _ = input in 0
      );
    let n_next = 
      if arg = 98 && n < 4
      then n + 1
      else n
    and i_next = i + 1
    in loop n_next i_next
  )