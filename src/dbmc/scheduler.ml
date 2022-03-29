open Core
open Lwt.Infix

type 'a job = { key : Lookup_key.t; payload : unit -> 'a Lwt.t }

let compare j1 j2 = -Lookup_key.compare j1.key j2.key

let create _map () =
  let cmp t1 t2 =
    Int.compare (Lookup_key.length t1.key) (Lookup_key.length t2.key)
  in
  Pairing_heap.create ~cmp ()

let push h key payload = Pairing_heap.add h { key; payload }
let pull h : 'a job option = Pairing_heap.pop h

let rec run ?(is_empty = false) q : 'a Lwt.t =
  Control_center.handle_available_commands () ;
  Lwt_mutex.lock Control_center.mutex >>= fun () ->
  Lwt_mutex.unlock Control_center.mutex ;
  Logs.debug (fun m -> m "[Queue]size = %d" (Pairing_heap.length q)) ;
  Logs.debug (fun m ->
      m "[Queue]%a"
        (Fmt.Dump.list Lookup_key.pp)
        (q |> Pairing_heap.to_list |> List.map ~f:(fun t -> t.key))) ;
  match pull q with
  | Some job ->
      (* ignore @@ job (); *)
      (* let%lwt _ = job () in *)
      Lwt.async job.payload ;
      Lwt_fmt.(flush stdout) ;%lwt
      Lwt.pause () ;%lwt
      run q
  | None ->
      if is_empty
      then Lwt.return_none
      else (
        Lwt.pause () ;%lwt
        run ~is_empty:true q)

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
