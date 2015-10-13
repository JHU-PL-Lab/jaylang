open Batteries;;

open Analysis_context_stack;;
open Cba_graph;;
open String_utils;;

module Stack : Context_stack =
struct
  type t = S of (abstract_clause list * Abs_clause_set.t);;
  let compare = compare;;
  let empty = S([],Abs_clause_set.empty);;
  let push c (S(c_list,c_set)) =
    if Abs_clause_set.mem c c_set
    then S( c :: (List.take_while (fun c' -> c <> c') c_list)
          , Abs_clause_set.singleton c)
    else S(c :: c_list, Abs_clause_set.add c c_set)
  ;;
  let pop (S(c_list,c_set)) =
    match c_list with
    | [] -> empty
    | h::t -> S(t, Abs_clause_set.remove h c_set)
  ;;
  let is_top c (S(c_list,_)) =
    match c_list with
    | [] -> true
    | h::_ -> c = h
  ;;
  let pretty (S(c_list,_)) =
    concat_sep "|" @@
    Enum.append
      (Enum.map pp_abstract_clause @@ List.enum c_list)
      (Enum.singleton "?")
  ;;
end;;