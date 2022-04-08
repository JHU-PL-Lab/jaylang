open Core
open Lwt.Infix

(* Lookup_result.ok_lwt x *)
(* These things exist together:
   1. a node for a lookup (key)
   2. a result stream for a lookup
   3. a producer for a lookup in the scheduler

   For a parent lookup, it should only be interested in the
   (1) to update the nodes relation in the global DAG
   and (2) to do its own processing.
   (3) is an initializaion. However, we cannot put it in the beginning
   of a lookup function. It's the force to create a lookup, an init-pre-init.

   In a complex lookup e.g. `r` at `r = f a`. The node and constraints are eagerly set.
   The lookup of `f` and `a` are lazy. However, it's possible `a` are eager
*)

module type S_sig = sig
  type message
  type result
  type key

  val equal_message : message -> message -> bool
  (* val init_task : key -> unit *)
end

module Make (Key : Base.Hashtbl.Key.S) (S : S_sig) = struct
  type key = Key.t
  type result = S.result
  type message = S.message
  type push = message option -> unit
  type handle0 = message -> unit
  type handle1 = push -> message -> unit
  type handle2 = push -> message -> message -> unit
  type task = unit -> unit

  type task_status =
    | Not_created
    | Initial
    (* | Waitng of task *)
    (* | Pending *)
    | Running
    | Done

  (* | Failed  *)
  (* used for cancellation *)

  (* TODO: consider thread-safety *)
  type detail = {
    stream : message Lwt_stream.t;
    push : push;
    mutable task_status : task_status;
    mutable messages : message list;
  }

  let is_status_initial = function Initial -> true | _ -> false
  let is_initial detail = is_status_initial detail.task_status

  let empty_detail () =
    let stream, push = Lwt_stream.create () in
    { task_status = Initial; stream; push; messages = [] }

  type t = { map : (Key.t, detail) Hashtbl.t }

  let push_mutex = Nano_mutex.create ()
  let create () : t = { map = Hashtbl.create (module Key) }

  let find_detail t key =
    match Hashtbl.find t.map key with
    | Some detail -> detail
    | None ->
        let detail = empty_detail () in
        Hashtbl.add_exn t.map ~key ~data:detail ;
        detail

  let find_detail_exn t key = Hashtbl.find_exn t.map key

  let get_stream t key =
    let detail = find_detail t key in
    Lwt_stream.clone detail.stream

  let create_task_stream t key task =
    let detail = find_detail t key in
    if is_initial detail
    then (
      task () ;
      detail.task_status <- Running)
    else ()

  let find_task_stream t key task =
    let detail = find_detail t key in
    if is_initial detail
    then (
      task () ;
      detail.task_status <- Running)
    else () ;
    Lwt_stream.clone detail.stream

  let find_push t key =
    let detail = find_detail t key in
    detail.push

  let push_fresh_result t key =
    let detail = find_detail t key in
    fun msg ->
      Nano_mutex.critical_section push_mutex ~f:(fun () ->
          if List.mem detail.messages msg ~equal:S.equal_message
          then ()
          else (
            detail.push (Some msg) ;
            detail.messages <- detail.messages @ [ msg ]))

  let on_lwt t key_src key_tgt task_tgt : unit Lwt.t =
    let stream_tgt = find_task_stream t key_tgt task_tgt in
    let cb = push_fresh_result t key_src in
    Lwt_stream.iter_p
      (fun x ->
        cb x ;
        Lwt.return_unit)
      stream_tgt

  let on t key_src key_tgt task_tgt : unit =
    Lwt.async (fun () -> on_lwt t key_src key_tgt task_tgt)

  let on_cb_lwt t _key_src key_tgt task_tgt cb : unit Lwt.t =
    let stream_tgt = find_task_stream t key_tgt task_tgt in
    Lwt_stream.iter_p (fun x -> cb x >>= fun () -> Lwt.return_unit) stream_tgt

  let on_answer_lwt t key_src key_tgt task_tgt answer : unit Lwt.t =
    let stream_tgt = find_task_stream t key_tgt task_tgt in
    let cb _ = push_fresh_result t key_src answer in
    Lwt_stream.iter_p
      (fun x ->
        cb x ;
        Lwt.return_unit)
      stream_tgt

  let on_answer t key_src key_tgt task_tgt answer : unit =
    Lwt.async (fun () -> on_answer_lwt t key_src key_tgt task_tgt answer)

  let all_lwt t key_src key_tgts =
    let cb = push_fresh_result t key_src in
    let stream_tgts = List.map key_tgts ~f:(get_stream t) in
    Lwt_list.iter_p
      (fun lookup_x_ret -> Lwt_stream.iter (fun x -> cb x) lookup_x_ret)
      stream_tgts

  let both t key_src key_tgt1 target_tgt1 key_tgt2 target_tgt2 answer : unit =
    let push = find_push t key_src in
    let stream_tgt1 = find_task_stream t key_tgt1 target_tgt1 in
    let stream_tgt2 = find_task_stream t key_tgt2 target_tgt2 in
    let cb _ = push_fresh_result t key_src answer in

    (* TODO: this logic is obviously incorrect *)
    let rec loop () =
      let r1 = Lwt_stream.get stream_tgt1 in
      let r2 = Lwt_stream.get stream_tgt2 in
      let%lwt v1, v2 = Lwt.both r1 r2 in
      match (v1, v2) with
      | Some v1, Some v2 ->
          cb (v1, v2) ;
          loop ()
      | _, _ -> Lwt.return_unit
      (*
      | None, None -> Lwt.return_unit
      | _, _ ->
        result_pusher (Lookup_result.ok x) ;
        loop ()
      *)
    in

    Lwt.async loop

  (* liftings *)

  (* external use *)
  let get_messages t key =
    Hashtbl.find_and_call t.map key
      ~if_found:(fun x -> x.messages)
      ~if_not_found:(fun _ -> [])
end