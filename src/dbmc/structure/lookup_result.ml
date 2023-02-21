open Core
open Lookup_status

type t = { status : Lookup_status.t; from : Lookup_key.t }

let default x = { status = Good; from = x }
let ok x = default x
(* let fail x = { (default x) with status = Fail } *)

(* is `complete` a separate last message or does it come with the last message *)
let complete x = { (default x) with status = Complete }
let status_as x status = { x with status }

let status_join x1 x2 =
  match (x1.status, x2.status) with
  | Fail, _ | _, Fail -> Fail
  | Complete, Complete -> Complete
  | _, _ -> Good

let is_ok r = Lookup_status.is_ok r.status
