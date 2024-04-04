open Core

type t = {
  key_to_expr : (Lookup_key.t, Z3.Expr.expr) Hashtbl.t;
  mutable next_key_i : int;
}
(** [t] is a mutable type to track how lookup keys relate to unique integers *)

let create () : t =
  { key_to_expr = Hashtbl.create (module Lookup_key); next_key_i = 0 }

let clear (x : t) : unit =
  Hashtbl.clear x.key_to_expr ;
  x.next_key_i <- 0

(* mutates [x] if the key is not found *)
let get_expr (x : t) (key : Lookup_key.t) var_i : Z3.Expr.expr =
  match Hashtbl.find x.key_to_expr key with
  | Some expr -> expr
  | None ->
      let expr = var_i x.next_key_i in
      (* make variable out of int *)
      Hashtbl.set x.key_to_expr ~key ~data:expr ;
      x.next_key_i <- x.next_key_i + 1 ;
      (* mark key as used and update to next unused key *)
      expr
