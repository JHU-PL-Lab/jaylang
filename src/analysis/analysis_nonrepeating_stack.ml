open Batteries;;

open Analysis_context_stack;;
open Analysis_data_structures;;
open Analysis_utils;;
open Ast;;
open Ast_pretty;;
open String_utils;;

module Stack : Context_stack =
struct
  type t = S of (clause list * Clause_set.t);;
  let compare = compare;;
  let empty = S([],Clause_set.empty);;
  let push c (S(c_list,c_set)) =
    if Clause_set.mem c c_set
    then S( c :: (List.take_while (fun c' -> c <> c') c_list)
          , Clause_set.singleton c)
    else S(c :: c_list, Clause_set.add c c_set)
  ;;
  let pop (S(c_list,c_set)) =
    match c_list with
    | [] -> empty
    | h::t -> S(t, Clause_set.remove h c_set)
  ;;
  let is_top c (S(c_list,_)) =
    match c_list with
    | [] -> true
    | h::_ -> c = h
  ;;
  let pretty (S(c_list,_)) =
    concat_sep "|" @@
    Enum.append
      (Enum.map pretty_clause @@ List.enum c_list)
      (Enum.singleton "?")
  ;;
  let enumerate e : t Enum.t =
    let all_clauses = Clause_set.of_enum @@ extract_context_clauses e in
    (* The possible context stacks are all permutations of this with any number
       of discarded elements. *)
    let rec pick_or_not choices : clause Enum.t Enum.t =
      choices
      |> Clause_set.enum
      |> Enum.map
        (fun c ->
           pick_or_not (Clause_set.remove c choices)
           |> Enum.map (fun e -> Enum.append (Enum.singleton c) @@ Enum.clone e)
        )
      |> Enum.concat
      |> Enum.append (Enum.singleton @@ Enum.empty ())
    in
    pick_or_not all_clauses
    |> Enum.map
      (fun e ->
         let l = List.of_enum e in
         let s = Clause_set.of_list l in
         S(l,s)
      )
  ;;
end;;