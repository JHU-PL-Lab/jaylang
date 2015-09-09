open Batteries;;

open Multimap;;
open String_utils;;

module type Sig =
sig
  module M : Multimap_sig
  val pp_key : M.key -> string
  val pp_value : M.value -> string
end;;

module Make(S : Sig) =
struct
  let pp m =
    let map_string =
      S.M.keys m
      |> Enum.map (fun k -> (k, S.M.find k m))
      |> Enum.map
        (fun (k,vs) ->
          S.pp_key k ^ " => " ^
          (concat_sep_delim "{" "}" ", " @@ Enum.map S.pp_value vs)
        )
      |> concat_sep ",\n"
    in
    "{\n" ^ indent 2 map_string ^ "\n}" 
  ;;
end;;
