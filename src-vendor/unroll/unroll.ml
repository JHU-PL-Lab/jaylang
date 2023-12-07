open Core
open Lwt.Infix
include Unroll_intf

(* Todo (for stateful stream):
   1. The stream is stateful.
   1.1 .. and the state is internal
   1.2 .. and the state is user-defined
   3. The message can be control or value.
   4. The state can propagate.
*)

(* These things exist together:
   1. a node for a lookup (key)
   2. a payload stream for a lookup
   3. a producer for a lookup in the scheduler

   For a parent lookup, it should only be interested in the
   (1) to update the nodes relation in the global DAG
   and (2) to do its own processing.
   (3) is an initializaion. However, we cannot put it in the beginning
   of a lookup function. It's the force to create a lookup, an init-pre-init.

   In a complex lookup e.g. `r` at `r = f a`. The node and constraints are eagerly set.
   The lookup of `f` and `a` are lazy. However, it's possible `a` are eager
*)

module N = Naive_state_machine

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
  S
    with type message = M.message
     and type payload = M.payload
     and type key = Key.t = struct
  module C = struct
    type key = Key.t
    type payload = M.payload
    type message = M.message
    type push = message option -> unit

    type detail = {
      stream : message Lwt_stream.t;
      push : push;
      mutable status : N.t;
      mutable pre_push : message -> message option;
      mutable messages : message list;
    }

    (* init *)

    let msg_queue = ref []

    let add_msg msg =
      msg_queue := msg :: !msg_queue ;
      msg

    type t = { map : (Key.t, detail) Hashtbl.t }

    let create () : t = { map = Hashtbl.create (module Key) }
    let reset state = Hashtbl.clear state.map
    let push_mutex = Nano_mutex.create ()

    let empty_detail () =
      let stream, push = Lwt_stream.create () in
      let pre_push m = Some m in
      { stream; push; status = Initial; pre_push; messages = [] }

    let find_detail t key = Hashtbl.find_or_add t.map key ~default:empty_detail

    let set_pre_push t key pre_push =
      let detail = find_detail t key in
      detail.pre_push <- pre_push

    let create_key t ?task key =
      let detail = find_detail t key in
      if N.is_status_initial detail.status
      then (
        detail.status <- Running ;
        match task with None -> () | Some t -> t ())
      else ()

    (* low-level *)

    let get_stream t key =
      let detail = find_detail t key in
      Lwt_stream.clone detail.stream

    let real_push t key =
      let detail = find_detail t key in
      fun msg ->
        Nano_mutex.critical_section push_mutex ~f:(fun () ->
            if List.mem detail.messages msg ~equal:M.equal_message
            then ()
            else
              (* Note this push is not for the current src-tgt pair.
                 The handler function belonging to this target key is defined at the place
                 where `real_push` is used.
                 This push is used for other pairs in which this target is their's source.
                 That's the reason why we cannot see any handler here.
              *)
              match detail.pre_push msg with
              | Some msg ->
                  detail.push (Some msg) ;
                  detail.messages <- detail.messages @ [ msg ]
              | None -> ())

    let real_close t key =
      let detail = find_detail t key in
      detail.push None

    let just_push t key v =
      match v with Some msg -> real_push t key msg | None -> real_close t key

    let push_all t key msgs = List.iter msgs ~f:(real_push t key)

    (* public *)

    let one_shot t key v =
      real_push t key v ;
      real_close t key ;
      Lwt.return_unit

    let by_iter t key_src f =
      let stream_src = get_stream t key_src in
      Lwt_stream.iter_s (fun x -> f x) stream_src

    let by_id t key_dst key_src : unit Lwt.t =
      let stream_src = get_stream t key_src in
      Lwt_stream.iter_s
        (fun x ->
          (real_push t key_dst) x ;
          Lwt.return_unit)
        stream_src

    let by_map t key_dst key_src f : unit Lwt.t =
      let stream_src = get_stream t key_src in
      Lwt_stream.iter_s
        (fun x ->
          real_push t key_dst (f x) ;
          Lwt.return_unit)
        stream_src

    let by_filter_map t key_dst key_src f : unit Lwt.t =
      let stream_src = get_stream t key_src in
      Lwt_stream.iter_s
        (fun x ->
          (match f x with Some v -> (real_push t key_dst) v | None -> ()) ;
          Lwt.return_unit)
        stream_src

    let by_bind t key_dst key_src f : unit Lwt.t =
      let stream_src = get_stream t key_src in
      Lwt_stream.iter_s (fun x -> f key_dst x) stream_src

    let by_join t ?(f = Fn.id) key_dst key_srcs =
      let cb = real_push t key_dst in
      let stream_srcs = List.map key_srcs ~f:(get_stream t) in
      Lwt_list.iter_p
        (fun lookup_x_ret -> Lwt_stream.iter (fun x -> cb (f x)) lookup_x_ret)
        stream_srcs

    (*
     let by_join_map t key_dst key_srcs f = by_join t ~f key_dst key_srcs

     let by_join_map_u t key_dst key_srcs f =
       Lwt.async (fun () -> by_join t ~f key_dst key_srcs) *)

    (* let product_stream_ s1 s2 =
       let s, f = Lwt_stream.create () in
       let rec loop () =
         let r1 = Lwt_stream.get s1 in
         let r2 = Lwt_stream.get s2 in
         let%lwt v1, v2 = Lwt.both r1 r2 in
         match (v1, v2) with
         | Some v1, Some v2 ->
             f (Some (v1, v2)) ;
             loop ()
         | _, _ -> Lwt.return_unit
       in
       Lwt.async loop ;
       s *)

    (* not used *)
    let product_stream s1 s2 =
      let s, f = Lwt_stream.create () in
      Lwt.async (fun () ->
          Lwt_stream.iter_s
            (fun v1 ->
              let s2' = Lwt_stream.clone s2 in
              Lwt_stream.iter_s
                (fun v2 ->
                  f (Some (v1, v2)) ;
                  Lwt.return_unit)
                s2')
            s1) ;
      s

    let by_map2 t key_dst key_src1 key_src2 f : unit Lwt.t =
      let stream_src1 = get_stream t key_src1 in
      let stream_src2 = get_stream t key_src2 in
      let cb = real_push t key_dst in

      Lwt_stream.iter_s
        (fun (v1, v2) ->
          cb (f (v1, v2)) ;
          Lwt.return_unit)
        (product_stream stream_src1 stream_src2)

    let by_filter_map2 t key_dst key_src1 key_src2 f : unit Lwt.t =
      let stream_src1 = get_stream t key_src1 in
      let stream_src2 = get_stream t key_src2 in
      let cb = real_push t key_dst in

      Lwt_stream.iter_s
        (fun (v1, v2) ->
          (match f (v1, v2) with Some v -> cb v | None -> ()) ;
          Lwt.return_unit)
        (product_stream stream_src1 stream_src2)

    (* TODO: this logic is obviously incorrect *)
    (* let rec loop () =
         let r1 = Lwt_stream.get stream_src1 in
         let r2 = Lwt_stream.get stream_src2 in
         let%lwt v1, v2 = Lwt.both r1 r2 in
         match (v1, v2) with
         | Some v1, Some v2 ->
             cb (f (v1, v2)) ;
             loop ()
         | _, _ -> Lwt.return_unit
       in
       loop () *)

    (* let by_join_both t key_dst key_src_pairs f : unit Lwt.t =
       let srcs =
         List.map key_src_pairs ~f:(fun (a, b) ->
             let s1, s2 = (get_stream t a, get_stream t b) in
             product_stream s1 s2)
       in
       let cb = real_push t key_dst in
       Lwt_list.iter_s
         (Lwt_stream.iter_s (fun v ->
              cb (f v) ;
              Lwt.return_unit))
         srcs *)

    (* external use *)
    let messages_sent t key =
      Hashtbl.find_and_call t.map key
        ~if_found:(fun x -> x.messages)
        ~if_not_found:(fun _ -> [])
  end

  include C

  module No_wait = struct
    include C

    let one_shot t key_src v =
      Lwt.async (fun () -> add_msg @@ one_shot t key_src v)

    let by_iter t key_src f =
      Lwt.async (fun () -> add_msg @@ by_iter t key_src f)

    let by_id t key_dst key_src =
      Lwt.async (fun () -> add_msg @@ by_id t key_dst key_src)

    let by_map t key_dst key_src f =
      Lwt.async (fun () -> add_msg @@ by_map t key_dst key_src f)

    let by_filter_map t key_dst key_src f =
      Lwt.async (fun () -> add_msg @@ by_filter_map t key_dst key_src f)

    let by_bind t key_dst key_src f =
      let f_lwt x s = Lwt.return (f x s) in

      Lwt.async (fun () -> add_msg @@ by_bind t key_dst key_src f_lwt)

    let by_join t ?(f = Fn.id) key_src key_dsts =
      Lwt.async (fun () -> add_msg @@ by_join t ~f key_src key_dsts)

    let by_map2 t key_dst key_src1 key_src2 f =
      Lwt.async (fun () -> add_msg @@ by_map2 t key_dst key_src1 key_src2 f)

    let by_filter_map2 t key_dst key_src1 key_src2 f =
      Lwt.async (fun () ->
          add_msg @@ by_filter_map2 t key_dst key_src1 key_src2 f)
  end
end

module Make_just_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) =
  Make
    (Key)
    (struct
      include P

      type message = payload

      let equal_message = equal_payload
    end)

module Make0 (Key : Base.Hashtbl.Key.S) (M : P_sig) :
  S2 with type payload = M.payload and type key = Key.t = struct
  type key = Key.t
  type payload = M.payload
  type control = unit
  type message = Payload of payload | Control of control
  type act = unit Lwt.t

  let equal_message _ _ = false
  let p2m p = Payload p
  let _not_here : message = Control ()

  (* type control = unit
     type msg = Payload of payload | Control of control *)
  type push = message option -> unit

  type detail = {
    stream : message Lwt_stream.t;
    push : push;
    mutable status : N.t;
    mutable pre_push : message -> message option;
    mutable messages : message list;
  }

  (* init *)

  let msg_queue = ref []

  type t = { map : (Key.t, detail) Hashtbl.t }

  let create () : t = { map = Hashtbl.create (module Key) }
  let reset state = Hashtbl.clear state.map
  let push_mutex = Nano_mutex.create ()

  let empty_detail () =
    let stream, push = Lwt_stream.create () in
    let pre_push m = Some m in
    { stream; push; status = Initial; pre_push; messages = [] }

  let find_detail t key = Hashtbl.find_or_add t.map key ~default:empty_detail

  let set_pre_push t key pre_push =
    let detail = find_detail t key in
    detail.pre_push <- pre_push

  let create_key t ?task key =
    let detail = find_detail t key in
    if N.is_status_initial detail.status
    then (
      detail.status <- Running ;
      match task with None -> () | Some t -> t ())
    else ()

  (* external use *)
  let messages_sent t key =
    Hashtbl.find_and_call t.map key
      ~if_found:(fun x -> x.messages)
      ~if_not_found:(fun _ -> [])

  (* low-level *)
  let get_stream t key =
    let detail = find_detail t key in
    Lwt_stream.clone detail.stream

  let real_push t key =
    let detail = find_detail t key in
    fun msg ->
      Nano_mutex.critical_section push_mutex ~f:(fun () ->
          if List.mem detail.messages msg ~equal:equal_message
          then ()
          else
            (* Note this push is not for the current src-tgt pair.
               The handler function belonging to this target key is defined at the place
               where `real_push` is used.
               This push is used for other pairs in which this target is their's source.
               That's the reason why we cannot see any handler here.
            *)
            match detail.pre_push msg with
            | Some msg ->
                detail.push (Some msg) ;
                detail.messages <- detail.messages @ [ msg ]
            | None -> ())

  let real_push_lwt t key msg =
    let push = real_push t key in
    push msg ;
    Lwt.return_unit

  (* let push_lwt t key p =
     let push = real_push t key in
     push (p2m p) ;
     Lwt.return_unit *)

  (* how to handle state is outside of f *)

  let lift_in f msg =
    match msg with Payload p -> f p | Control _ -> Lwt.return_unit

  let map_in f msg =
    match msg with Payload p -> Some (Payload (f p)) | Control _ -> None

  let map_out f msg = match msg with Some p -> f p | None -> ()

  let map_in_opt f_opt msg =
    match msg with
    | Payload p -> (
        match f_opt p with Some v -> Some (Payload v) | None -> None)
    | Control _ -> None

  let map2_in f (msg1, msg2) =
    match (msg1, msg2) with
    | Payload p1, Payload p2 -> Some (Payload (f (p1, p2)))
    | _, _ -> None

  let map2_in_opt f_opt (msg1, msg2) =
    match (msg1, msg2) with
    | Payload p1, Payload p2 -> (
        match f_opt (p1, p2) with Some v -> Some (Payload v) | None -> None)
    | _, _ -> None

  let seq f push msg = msg |> map_in f |> map_out push
  let seq_opt f_opt push msg = msg |> map_in_opt f_opt |> map_out push
  let seq2 f push (msg1, msg2) = (msg1, msg2) |> map2_in f |> map_out push

  let seq2_opt f_opt push (msg1, msg2) =
    (msg1, msg2) |> map2_in_opt f_opt |> map_out push

  let then_lwt f msg =
    f msg ;
    Lwt.return_unit

  let real_close t key =
    let detail = find_detail t key in
    detail.push None

  let just_push t key v =
    match v with Some msg -> real_push t key msg | None -> real_close t key

  let push_all t key msgs = List.iter msgs ~f:(real_push t key)

  (* public *)

  let one_shot t key v =
    real_push t key (p2m v) ;
    real_close t key ;
    Lwt.return_unit

  let by_iter t key_src f =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (lift_in f) stream_src

  let by_id t key_dst key_src =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (real_push_lwt t key_dst) stream_src

  let by_map t key_dst key_src f =
    let stream_src = get_stream t key_src in
    let push = real_push t key_dst in
    Lwt_stream.iter_s (then_lwt @@ seq f push) stream_src

  let by_filter_map t key_dst key_src f_opt =
    let stream_src = get_stream t key_src in
    let push = real_push t key_dst in
    Lwt_stream.iter_s (then_lwt @@ seq_opt f_opt push) stream_src

  let by_bind t key_dst key_src f =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (lift_in (f key_dst)) stream_src

  let by_join t ?(f = Fn.id) key_dst key_srcs =
    let stream_srcs = List.map key_srcs ~f:(get_stream t) in
    let push = real_push t key_dst in
    Lwt_list.iter_p (Lwt_stream.iter (seq f push)) stream_srcs

  let product_stream s1 s2 =
    let s, f = Lwt_stream.create () in
    Lwt.async (fun () ->
        Lwt_stream.iter_s
          (fun v1 ->
            let s2' = Lwt_stream.clone s2 in
            Lwt_stream.iter_s
              (fun v2 ->
                f (Some (v1, v2)) ;
                Lwt.return_unit)
              s2')
          s1) ;
    s

  let by_map2 t key_dst key_src1 key_src2 f : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in
    let stream12 = product_stream stream_src1 stream_src2 in
    let push = real_push t key_dst in
    Lwt_stream.iter_s (then_lwt @@ seq2 f push) stream12

  let by_filter_map2 t key_dst key_src1 key_src2 f_opt : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in
    let stream12 = product_stream stream_src1 stream_src2 in
    let push = real_push t key_dst in
    Lwt_stream.iter_s (then_lwt @@ seq2_opt f_opt push) stream12
end
