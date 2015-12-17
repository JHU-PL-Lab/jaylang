open Batteries;;

open Analysis_context_stack;;
open Ddpa_graph;;

type 'a dq = 'a Deque.dq;;

module type Spec =
sig
  val size : int
end;;

module Make(S : Spec) : Context_stack =
struct
  type t = abstract_clause dq;;
  let compare x y = Enum.compare compare (Deque.enum x) (Deque.enum y);;
  let empty = Deque.empty;;
  let push c x =
    let x' = Deque.cons c x in
    if Deque.size x' > S.size
    then fst @@ Option.get @@ Deque.rear x'
    else x'
  ;;
  let pop x =
    match Deque.front x with
    | None -> Deque.empty
    | Some(_,x') -> x'
  ;;
  let is_top c x =
    match Deque.front x with
    | None -> true
    | Some(c',_) -> c = c'
  ;;
  let pretty x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map pp_abstract_clause @@ Deque.enum x
  ;;
  let pretty_abbrv x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map ppa_abstract_clause @@ Deque.enum x
  ;;
  let name = string_of_int S.size ^ "ddpa";;
end;;

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
