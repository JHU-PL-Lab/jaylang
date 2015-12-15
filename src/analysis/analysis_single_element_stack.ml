open Batteries;;

open Analysis_context_stack;;
open Ddpa_graph;;

module Stack : Context_stack =
struct
  type t = S of abstract_clause option;;
  let compare = compare;;
  let empty = S(None);;
  let push c _ = S(Some(c));;
  let pop _ = S(None);;
  let is_top c (S(c_option)) =
    match c_option with
    | Some c' -> c = c'
    | None -> true
  ;;
  let pretty = function
    | S(Some(c)) -> pp_abstract_clause c ^ "|?"
    | S(None) -> "?"
  ;;
  let pretty_abbrv = function
    | S(Some(c)) -> ppa_abstract_clause c
    | S(None) -> "?"
  ;;
  let name = "1ddpa";;
end;;