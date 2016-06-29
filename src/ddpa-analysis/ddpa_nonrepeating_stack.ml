open Batteries;;

open Ddpa_context_stack;;
open Ddpa_graph;;
open Pp_utils;;

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
  let pp formatter (S(c_list,_)) =
    c_list
    |> List.iter ((pp_suffix pp_var_of_abstract_clause "|") formatter);
    Format.pp_print_string formatter "?"
  ;;
  let show = pp_to_string pp;;
  let name = "ddpaNR";;
end;;
