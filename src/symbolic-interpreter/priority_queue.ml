(** This module defines a value-parametric priority queue.  The algorithm was
    derived from v4.08 of The OCaml System
    (http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html). *)
(* In accordance with the license of that document, the license for the original
   priority queue source appears below. *)
(*
The OCaml system is copyright © 1996–2013 Institut National de Recherche en Informatique et en Automatique (INRIA). INRIA holds all ownership rights to the OCaml system.

The OCaml system is open source and can be freely redistributed. See the file LICENSE in the distribution for licensing information.

The present documentation is copyright © 2013 Institut National de Recherche en Informatique et en Automatique (INRIA). The OCaml documentation and user’s manual may be reproduced and distributed in whole or in part, subject to the following conditions:

  * The copyright notice above and this permission notice must be preserved complete on all complete or partial copies.
  * Any translation or derivative work of the OCaml documentation and user’s manual must be approved by the authors in writing before distribution.
  * If you distribute the OCaml documentation and user’s manual in part, instructions for obtaining the complete version of this manual must be included, and a means for obtaining a complete version provided.
  * Small portions may be reproduced as illustrations for reviews or quotes in other works without this permission notice if proper citation is given.
*)

open Batteries;;

(** The signature of a priority queue.  The given priority orders the enqueued
    elements.  Values with a lower priority value are dequeued first. *)
module type PQ = sig
  module Priority : Interfaces.OrderedType;;
  type 'a t;;
  val empty : 'a t;;
  val is_empty : 'a t -> bool;;
  val size : 'a t -> int;;
  val enqueue : Priority.t -> 'a -> 'a t -> 'a t;;
  val dequeue : 'a t -> (Priority.t * 'a * 'a t) option
end;;

module Make(Priority : Interfaces.OrderedType)
  : PQ with module Priority = Priority =
struct
  module Priority = Priority;;
  type 'a tree = Empty | Node of Priority.t * 'a * 'a tree * 'a tree;;
  type 'a t =
    { pq_size : int;
      pq_tree : 'a tree;
    }
  ;;
  let empty = { pq_size = 0; pq_tree = Empty };;
  let is_empty pq = pq.pq_size = 0;;
  let size pq = pq.pq_size;;
  let enqueue prio elt pq =
    let rec add prio elt tree =
      match tree with
      | Empty  -> Node(prio, elt, Empty, Empty)
      | Node(prio', elt', left, right) ->
        if Priority.compare prio prio' < 0 then
          Node(prio, elt, add prio' elt' right, left)
        else
          Node(prio', elt', add prio elt right, left)
    in
    { pq_size = pq.pq_size + 1;
      pq_tree = add prio elt pq.pq_tree
    }
  ;;
  let dequeue (pq : 'a t) : (Priority.t * 'a * 'a t) option =
    let rec remove (x : 'a tree) : 'a tree =
      match x with
      | Empty -> raise @@ Jhupllib.Utils.Invariant_failure "remove from empty pq"
      | Node(_, _, Empty, Empty) -> Empty
      | Node(_, _, left, Empty) -> left
      | Node(_, _, Empty, right) -> right
      | Node(_, _,
             ((Node(lprio, lelt, _, _)) as left),
             ((Node(rprio, relt, _, _)) as right)) ->
        if Priority.compare lprio rprio < 0 then
          Node(lprio, lelt, remove left, right)
        else
          Node(rprio, relt, left, remove right)
    in
    match pq.pq_tree with
    | Empty -> None
    | Node(prio, elt, _, _) ->
      let pq' =
        { pq_size = pq.pq_size + 1;
          pq_tree = remove pq.pq_tree;
        }
      in
      Some(prio, elt, pq')
  ;;
end;;
