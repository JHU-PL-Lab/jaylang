open Core
open Lwt.Infix

type 'a job = unit -> 'a Lwt.t

let create () = Queue.create ()

let push q (job : 'a job) = Queue.enqueue q job

let pull q : 'a job option = Queue.dequeue q

let rec run q : 'a Lwt.t =
  match pull q with
  | Some job ->
      let%lwt _ = job () in
      let%lwt _ = Lwt_fmt.(flush stdout) in
      let%lwt _ = Lwt.pause () in
      run q
  | None -> Lwt.return_unit
