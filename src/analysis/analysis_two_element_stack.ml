open Batteries;;

open Analysis_context_stack;;
open Analysis_utils;;
open Ast;;
open Ast_pretty;;

module Stack : Context_stack =
struct
  type t = S of clause list;;
  let compare = compare;;
  let empty = S([]);;
  let push c (S(lst)) =
    match lst with
      | [] -> S([c])
      | h::_ -> S([c;h])
  ;;
  let pop (S(lst)) =
    match lst with
      | [] -> S([])
      | _::t -> S(t)
  ;;
  let is_top c (S(c_option)) =
    match c_option with
    | h::_ -> c = h
    | [] -> true
  ;;
  let pretty (S(lst)) =
    let pfx = List.fold_left (fun a c -> a ^ pretty_clause c ^ "|") "" lst in
    pfx ^ "?"
  ;;
  let enumerate e : t Enum.t =
    let context_clauses = extract_context_clauses e in
    let context_clauses' = Enum.clone context_clauses in
    context_clauses
    |> Enum.map
        (fun c ->
          Enum.clone context_clauses'
          |> Enum.map (fun c' -> [c;c'])
          |> Enum.append (Enum.singleton [c])
        )
    |> Enum.concat
    |> Enum.append (Enum.singleton [])
    |> Enum.map (fun x -> S(x))
  ;;
end;;