open Batteries;;

open Multimap;;
open Pp_utils;;

module type Sig =
sig
  module M : Multimap_sig
  val pp_key : M.key pretty_printer
  val pp_value : M.value pretty_printer
end;;

module Make(S : Sig) =
struct
  let pp formatter m =
    let pp_values formatter vs =
      pp_concat_sep_delim "{" "}" "," S.pp_value formatter vs
    in
    let pp_mapping formatter (k,vs) =
      Format.fprintf formatter "%a =>@ %a" S.pp_key k pp_values vs
    in
    pp_concat_sep_delim "{" "}" "," pp_mapping formatter
      (S.M.keys m |> Enum.map (fun k -> (k, S.M.find k m)))
  ;;
  let show = pp_to_string pp;;
end;;
