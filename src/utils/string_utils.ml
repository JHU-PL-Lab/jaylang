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

let indent n s =
  String.replace_chars
    (fun x -> if x = '\n' then "\n" ^ String.make n ' ' else String.of_char x)
    s
;;

let rec whitespace_split ?max:(n=max_int) s =
  if n <= 1
  then [s]
  else
    try
      let (s1,s2) = String.split (String.trim s) ~by:" " in
      s1::(whitespace_split ~max:(n-1) s2)
    with
    | Not_found -> [s]
;;

    