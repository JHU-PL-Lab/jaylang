open Core

type t = { status : bool; from : Lookup_key.t; is_complete : bool }

let default x = { status = true; from = x; is_complete = false }
let ok x = default x
let fail x = { (default x) with status = false }
let ok_lwt x = Lwt.return (ok x)
let fail_lwt x = Lwt.return (fail x)
