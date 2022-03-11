open Core
open Lwt.Infix

type 'a job = unit -> 'a Lwt.t
(* type sentinel = Yield *)

let make_sentinel () : 'a Lwt.t * 'a Lwt.u = Lwt.wait ()

(*
   One Lwt.t can wait for the other Lwt.t.
   A job can create a Lwt.t in the future.
   The job is not a Lwt.t.
   If the job isn't scheduled, there is no Lwt.t at all.
   We cannot create the Lwt.t in advance, because any created Lwt.t will be
   scheduled by its internal loop. We lose the control of scheduling.

   However, we can create a sentinel Lwt.t and wait for that.
   The sentinel Lwt.t is created in the last task in `wait_all`.
*)

exception EmptyTaskQueue
exception EmptyTaskList

let create () = Queue.create ()
let push q (job : 'a job) = Queue.enqueue q job
let pull q : 'a job option = Queue.dequeue q

(*
   Can a queue be empty? Should it raise an exception when it's empty?
*)
let rec run ?(is_empty = false) q : 'a Lwt.t =
  let%lwt _ = Logs_lwt.app (fun m -> m "Queue size= %d" (Queue.length q)) in
  match pull q with
  | Some job ->
      ignore @@ job ();
      let%lwt _ = Lwt_fmt.(flush stdout) in
      let%lwt _ = Lwt.pause () in
      run q
  | None ->
      if is_empty
      then Lwt.return_none
      else
        let%lwt _ = Lwt.pause () in
        run ~is_empty:true q
(* raise EmptyTaskQueue *)
(* Lwt.return_none *)

let wait_all dummy q jobs : _ Lwt.t =
  let follow t1 t2 () =
    let%lwt r1 = t1 () in
    push q t2;
    (* Lwt.return_unit *)
    Lwt.return r1
  in
  if List.length jobs = 0
  then dummy
  else
    let sentinel_p, sentinel_r = make_sentinel () in
    let jobs = List.rev jobs in
    let h_job, tl_jobs = (List.hd_exn jobs, List.tl_exn jobs) in
    let h'_job () =
      let%lwt rh = h_job () in
      Lwt.wakeup_later sentinel_r rh;
      let%lwt _ = Lwt.pause () in
      sentinel_p
    in
    let task = List.reduce_exn (h'_job :: tl_jobs) ~f:(Fn.flip follow) in
    push q task;
    let%lwt _ = Lwt.pause () in
    let%lwt rs = sentinel_p in
    Lwt.return rs

let add_and_detach q job : unit = push q job
