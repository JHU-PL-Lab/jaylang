open Batteries;;
open Odefa_ast;;

open Ast;;
open Ast_pp;;

(** The type of relative stacks in the symbolic interpreter. *)
type relative_stack =
  | Relative_stack of clause list * clause list
[@@deriving eq, ord, show, to_yojson]
;;

let empty = Relative_stack([],[]);;

(** FIXME: This does not match the spec in the paper, but the spec seems silly.
    Discuss and then resolve. *)
let push (Relative_stack(costk,stk)) (c : clause) : relative_stack option =
  match costk with
  | [] ->
    Some(Relative_stack(costk, c :: stk))
  | c' :: costk' ->
    if equal_clause c c' then Some(Relative_stack(costk', stk)) else None
;;

let pop (Relative_stack(costk,stk)) (c : clause) : relative_stack option =
  match stk with
  | [] ->
    Some(Relative_stack(c :: costk, stk))
  | c' :: stk' ->
    if equal_clause c c' then Some(Relative_stack(costk, stk')) else None
;;

let may_be_top (Relative_stack(_,stk)) (c : clause) : bool =
  match stk with
  | [] -> true (* because we have no idea what's on top of the stack now *)
  | c' :: _ -> equal_clause c c' (* because c' is definitely on top *)
;;
