open Core
open Dj_common

(* val job_key_compare : Lookup_key.t -> Lookup_key.t -> int *)
(* let job_key_compare (t1 : Lookup_key.t) (t2 : Lookup_key.t) =
   Int.compare (Lookup_key.length t1) (Lookup_key.length t2) *)

let job_key_compare (jk1 : Job_key.t) (jk2 : Job_key.t) =
  let visits_compare = Int.compare jk1.block_visits jk2.block_visits in
  if visits_compare <> 0
  then visits_compare
  else Int.compare (Lookup_key.length jk1.lookup) (Lookup_key.length jk2.lookup)

let create () = Scheduler.create ~cmp:job_key_compare ()