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

let pretty_list : 'a. ('a -> string) -> 'a list -> string =
  fun pretty_el lst ->
    concat_sep_delim "[" "]" ";" @@ List.enum @@ List.map pretty_el lst
;;

let pretty_tuple : 'a 'b. ('a -> string) -> ('b -> string) -> 'a * 'b -> string
  = fun pretty_a pretty_b (a,b) ->
    "(" ^ pretty_a a ^ ", " ^ pretty_b b ^ ")"
;;