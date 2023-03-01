open Core
open Lookup_status

type t = { status : Lookup_status.t; from : Lookup_key.t }

let default x = { status = Good; from = x }
let ok x = default x

(* let fail x = { (default x) with status = Fail } *)

(* is `complete` a separate last message or does it come with the last message *)
let complete x = { (default x) with status = Complete }
let status_as x status = { x with status }
let from_as from status = { from; status }
let is_ok r = Lookup_status.is_ok r.status
