open Core

module type S_sig = sig
  type message
  type result
  type key

  val equal_message : message -> message -> bool
  (* val init_task : key -> unit *)
end

module Make (Key : Base.Hashtbl.Key.S) (S : S_sig with type key = Key.t) =
struct
  type key = Key.t
  type result = S.result
  type message = S.message
  type push = message option -> unit
  type handle_1 = push -> message -> unit
  type handle_2 = push -> message -> message -> unit

  type task_status =
    | Not_created
    | Created
    | Running
    | Done
    | Failed (* used for cancellation *)

  (* TODO: consider thread-safety *)
  type t = {
    (* task_map : (Key.t, result) Hashtbl.t; *)
    stream_map : (Key.t, message Lwt_stream.t) Hashtbl.t;
    push_map : (Key.t, push) Hashtbl.t;
    message_map : (Key.t, message list) Hashtbl.t;
  }

  let push_mutex = Nano_mutex.create ()

  let create () : t =
    {
      (* task_map = Hashtbl.create (module Key); *)
      stream_map = Hashtbl.create (module Key);
      push_map = Hashtbl.create (module Key);
      message_map = Hashtbl.create (module Key);
    }

  (* let create_task t key = S.init_task key *)
  (* let find_task t key = Hashtbl.find_exn t.task_map key *)

  let find_stream_ext t key =
    match Hashtbl.find t.stream_map key with
    | Some s -> (true, Lwt_stream.clone s)
    | None ->
        let s, f = Lwt_stream.create () in
        Hashtbl.add_exn t.stream_map ~key ~data:s ;
        Hashtbl.add_exn t.push_map ~key ~data:f ;
        Hashtbl.add_exn t.message_map ~key ~data:[] ;
        (false, Lwt_stream.clone s)

  let find_stream t key = find_stream_ext t key |> snd
  let find_messages_exn t key = Hashtbl.find_exn t.message_map key
  let find_messages t key = Hashtbl.find t.message_map key
  let find_push t key = Hashtbl.find_exn t.push_map key

  let push_fresh_result t key_src =
    let push = find_push t key_src in
    fun msg ->
      Nano_mutex.critical_section push_mutex ~f:(fun () ->
          let messages = find_messages_exn t key_src in
          if List.mem messages msg ~equal:S.equal_message
          then ()
          else (
            push (Some msg) ;
            Hashtbl.update t.message_map key_src ~f:(function
              | Some _ -> messages @ [ msg ]
              | None -> failwith "non-exist messages")))

  let on t key_src key_tgt (cb1 : handle_1) : unit Lwt.t =
    let stream_tgt = find_stream t key_tgt in
    let cb = cb1 (find_push t key_src) in
    Lwt_stream.iter_p
      (fun x ->
        cb x ;
        Lwt.return_unit)
      stream_tgt

  let both t key_src key_tgt1 key_tgt2 (cb2 : handle_2) : unit =
    let push = find_push t key_src in
    let stream_tgt1 = find_stream t key_tgt1 in
    let stream_tgt2 = find_stream t key_tgt2 in
    let cb = cb2 (find_push t key_src) in

    (* TODO: this logic is obviously incorrect *)
    let rec loop () =
      let r1 = Lwt_stream.get stream_tgt1 in
      let r2 = Lwt_stream.get stream_tgt2 in
      let%lwt v1, v2 = Lwt.both r1 r2 in
      match (v1, v2) with
      | Some v1, Some v2 ->
          cb v1 v2 ;
          loop ()
      | _, _ -> Lwt.return_unit
    in

    Lwt.async loop

  (* liftings *)
end