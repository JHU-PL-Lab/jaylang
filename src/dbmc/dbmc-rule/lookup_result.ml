open Core

(*
   (solo) status lattice:

   Complete  Fail
       \      /
        \    /
         Good---
           |   |
           -----
*)

type status = Good | Fail | Complete
type t = { status : status; from : Lookup_key.t }

let status_join s1 s2 =
  match (s1, s2) with
  | Fail, _ | _, Fail -> Fail
  | Complete, Complete -> Complete
  | _, _ -> Good

let default x = { status = Good; from = x }
let ok x = default x
(* let fail x = { (default x) with status = Fail } *)

(* is `complete` a separate last message or does it come with the last message *)
let complete x = { (default x) with status = Complete }
let is_ok x = match x.status with Good -> true | Complete -> true | _ -> false
let status_as x status = { x with status }
