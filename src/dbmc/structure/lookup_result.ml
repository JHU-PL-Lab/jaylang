open Core

type t = { status : bool; from : Id.t; mutable subs : (unit -> unit) list }

let default x = { status = true; from = x; subs = [] }
let ok x = default x
let fail x = { (default x) with status = false }
let ok_lwt x = Lwt.return (ok x)
let fail_lwt x = Lwt.return (fail x)
let result_seq _state _key : _ Lwt_seq.t = Lwt_seq.empty
