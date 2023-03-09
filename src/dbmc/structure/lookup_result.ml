open Core
open Lookup_status

type t = { status : Lookup_status.t; from : Lookup_key.t }

let pp oc r =
  Fmt.pf oc "%a(%a)" Lookup_key.pp r.from Lookup_status.pp_short r.status

let complete from = { from; status = Complete }
let fail from = { from; status = Fail }
let good from = { from; status = Good }
let from_as from status = { from; status }
