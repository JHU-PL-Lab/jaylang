open Batteries;;

open Analysis_context_stack;;
open Analysis_utils;;
open Ast;;
open Ast_pretty;;

module Stack : Context_stack =
struct
  type t = S of clause option;;
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
    | S(Some(c)) -> brief_pretty_clause c ^ "|?"
    | S(None) -> "?"
  ;;
  let enumerate e : t Enum.t =
    extract_context_clauses e
    |> Enum.map (fun c -> S(Some c))
    |> Enum.append (Enum.singleton @@ S(None))
  ;;
end;;