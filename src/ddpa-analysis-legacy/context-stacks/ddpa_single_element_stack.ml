open Batteries;;
open Jhupllib;;

open Ddpa_abstract_ast;;
open Ddpa_context_stack;;
open Pp_utils;;

module Stack : Context_stack =
struct
  type t = S of abstract_clause option;;
  let equal x y = compare x y == 0;;
  let compare = compare;;
  let empty = S(None);;
  let push c _ = S(Some(c));;
  let pop _ = S(None);;
  let is_top c (S(c_option)) =
    match c_option with
    | Some c' -> c = c'
    | None -> true
  ;;
  let pp formatter x =
    match x with
    | S(Some(c)) -> (pp_suffix pp_abstract_clause "|?") formatter c
    | S(None) -> Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let to_yojson c =
    `List (
      match c with
      | S(Some(c)) -> [abstract_clause_to_yojson c]
      | S(None) -> []
    )
  ;;
  let name = "1ddpa";;
end;;
