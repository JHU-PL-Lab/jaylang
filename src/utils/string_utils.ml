open Batteries;;

let concat_sep sep strs =
  Enum.fold
    (fun acc s ->
       acc ^
       (if String.is_empty acc then "" else sep) ^
       s
    ) "" strs
;;

let concat_sep_delim start stop sep strs =
  start ^ concat_sep sep strs ^ stop
;;

let pp_list : 'a. ('a -> string) -> 'a list -> string =
  fun pp_el lst ->
    concat_sep_delim "[" "]" ";" @@ List.enum @@ List.map pp_el lst
;;

let pp_tuple : 'a 'b. ('a -> string) -> ('b -> string) -> 'a * 'b -> string
  = fun pp_a pp_b (a,b) ->
    "(" ^ pp_a a ^ ", " ^ pp_b b ^ ")"
;;