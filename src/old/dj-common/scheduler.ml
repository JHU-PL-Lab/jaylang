open Core
open Lwt.Infix

(* This scheduler will use `Lwt.async_exception_hook` so for safety
   only one instance should be used at one time *)

(* Scheduler doesn't need to maintain a key map to make it unique.
   For a scheduler, it should only require a heap that can pop a task when needed.
   This heap can have duplicate keys.
*)

type ('key, 'r) job = { key : 'key; payload : unit -> 'r Lwt.t }

type ('key, 'r) t = {
  heap : ('key, 'r) job Pairing_heap.t;
  mutable is_complete : bool;
}

let create ~cmp () =
  let cmp j1 j2 = cmp j1.key j2.key in
  { heap = Pairing_heap.create ~cmp (); is_complete = false }

let reset s = Pairing_heap.clear s.heap
let set_complete s = s.is_complete <- true
let push s key payload = Pairing_heap.add s.heap { key; payload }
let pull s : ('key, 'r) job option = Pairing_heap.pop s.heap

let rec run s : 'a Lwt.t =
  if s.is_complete
  then Lwt.return_none
  else
    match pull s with
    | Some job ->
        let guarded_payload () =
          if not s.is_complete then job.payload () else Lwt.return_unit
        in
        Lwt.async guarded_payload ;
        Lwt.pause () ;%lwt
        run s
    | None ->
        s.is_complete <- true ;
        Lwt.pause () ;%lwt
        run s

(*
Can a queue be empty? Should it raise an exception when it's empty?

   One Lwt.t can wait for the other Lwt.t.
   A job can create a Lwt.t in the future.
   The job is not a Lwt.t.
   If the job isn't scheduled, there is no Lwt.t at all.
   We cannot create the Lwt.t in advance, because any created Lwt.t will be
   scheduled by its internal loop. We lose the control of scheduling.

   However, we can create a sentinel Lwt.t and wait for that.
   The sentinel Lwt.t is created in the last task in `wait_all`.
*)
(*
   let rec run ?(last_run = false) s : 'a Lwt.t =
     (* Control_center.handle_available_commands () ;
        Lwt_mutex.lock Control_center.mutex >>= fun () ->
        Lwt_mutex.unlock Control_center.mutex ; *)
*)
(*
   exception EmptyTaskQueue
   exception EmptyTaskList 

   let make_sentinel () : 'a Lwt.t * 'a Lwt.u = Lwt.wait () *)

(* let wait_all dummy q jobs : _ Lwt.t =
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
     Lwt.return rs *)
