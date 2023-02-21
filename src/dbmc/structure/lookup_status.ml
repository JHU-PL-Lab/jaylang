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

type t = Good | Fail | Complete

let show = function Good -> "good" | Fail -> "fail" | Complete -> "done"
let pp = Fmt.(using show string)
let is_ok = function Good | Complete -> true | _ -> false
(* let is_good = function Good -> true | _ -> false *)
(* let is_complete x = match x.status with Complete -> true | _ -> false

   let is_complete_or_fail x =
     match x.status with Complete | Fail -> true | _ -> false *)
