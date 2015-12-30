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
  let pp x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map pp_abstract_clause @@ Deque.enum x
  ;;
  let ppa x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map ppa_abstract_clause @@ Deque.enum x
  ;;
  let name = string_of_int S.size ^ "ddpa";;
end;;
