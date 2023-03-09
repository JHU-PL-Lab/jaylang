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

type t = Good | Fail | Complete [@@deriving equal]

let show = function Good -> "good" | Fail -> "fail" | Complete -> "C"
let pp = Fmt.(using show string)
let show_short = function Good -> "G" | Fail -> "F" | Complete -> "C"
let pp_short = Fmt.(using show_short string)
let is_complete = function Complete -> true | _ -> false
let is_complete_or_fail = function Complete | Fail -> true | _ -> false

let leq s s' =
  match (s, s') with
  | Good, _ -> true
  | Complete, Complete -> true
  | Fail, Fail -> true
  | _ -> false

let iter ~good ~complete ~fail = function
  | Good -> good ()
  | Complete -> complete ()
  | Fail -> fail ()

let iter_good r f = iter ~good:f ~complete:Fn.ignore ~fail:Fn.ignore r
let iter_ok r f = iter ~good:f ~complete:f ~fail:Fn.ignore r

(* let is_good = function Good -> true | _ -> false *)
(* let is_complete x = match x.status with Complete -> true | _ -> false

   *)

let join x1 x2 =
  match (x1, x2) with
  | Fail, _ | _, Fail -> Fail
  | Complete, Complete -> Complete
  | _, _ -> Good

let complete_or_fail b = if b then Complete else Fail
