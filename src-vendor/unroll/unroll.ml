open Core
open Lwt.Infix
include Unroll_intf

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

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig with type key = Key.t) :
  S
    with type message = M.message
     and type result = M.result
     and type key = M.key = struct
  type key = Key.t
  type result = M.result
  type message = M.message
  type push = message option -> unit

  type task_status =
    (* | Not_created *)
    | Initial
    (* | Waitng of task *)
    (* | Pending *)
    | Running
  (* | Done *)

  (* | Failed  *)
  (* used for cancellation *)

  (* TODO: consider thread-safety *)
  type detail = {
    stream : message Lwt_stream.t;
    push : push;
    mutable pre_push : message -> message option;
    mutable task_status : task_status;
    mutable messages : message list;
  }

  let is_status_initial = function Initial -> true | _ -> false
  let is_initial detail = is_status_initial detail.task_status

  let empty_detail () =
    let stream, push = Lwt_stream.create () in
    let pre_push m = Some m in
    { task_status = Initial; stream; push; pre_push; messages = [] }

  type t = { map : (Key.t, detail) Hashtbl.t }

  let create () : t = { map = Hashtbl.create (module Key) }
  let push_mutex = Nano_mutex.create ()

  let find_detail t key =
    match Hashtbl.find t.map key with
    | Some detail -> detail
    | None ->
        let detail = empty_detail () in
        Hashtbl.add_exn t.map ~key ~data:detail ;
        detail

  let get_stream t key =
    let detail = find_detail t key in
    Lwt_stream.clone detail.stream

  let set_pre_push t key pre_push =
    let detail = find_detail t key in
    detail.pre_push <- pre_push

  let alloc_task t ?task key =
    let detail = find_detail t key in
    if is_initial detail
    then (
      detail.task_status <- Running ;
      match task with None -> () | Some t -> t ())
    else ()

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

  let by_return t key v =
    let pusher = real_push t key in
    pusher v

  let by_iter t key_src f =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (fun x -> f x) stream_src

  let by_iter_u t key_src f = Lwt.async (fun () -> by_iter t key_src f)

  let by_id t key_tgt key_src : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s
      (fun x ->
        (real_push t key_tgt) x ;
        Lwt.return_unit)
      stream_src

  let by_id_u t key_tgt key_src : unit =
    Lwt.async (fun () -> by_id t key_tgt key_src)

  let by_map t key_tgt key_src f : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s
      (fun x ->
        real_push t key_tgt (f x) ;
        Lwt.return_unit)
      stream_src

  let by_map_u t key_tgt key_src f : unit =
    Lwt.async (fun () -> by_map t key_tgt key_src f)

  let by_filter_map t key_tgt key_src f : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s
      (fun x ->
        (match f x with Some v -> (real_push t key_tgt) v | None -> ()) ;
        Lwt.return_unit)
      stream_src

  let by_filter_map_u t key_tgt key_src f : unit =
    Lwt.async (fun () -> by_filter_map t key_tgt key_src f)

  let by_bind t key_tgt key_src f : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (fun x -> f key_tgt x) stream_src

  let by_bind_u t key_tgt key_src f : unit =
    Lwt.async (fun () -> by_bind t key_tgt key_src f)

  let by_join t ?(f = Fn.id) key_src key_tgts =
    let cb = real_push t key_src in
    let stream_tgts = List.map key_tgts ~f:(get_stream t) in
    Lwt_list.iter_s
      (fun lookup_x_ret -> Lwt_stream.iter (fun x -> cb (f x)) lookup_x_ret)
      stream_tgts

  let by_join_u t key_src key_tgts =
    Lwt.async (fun () -> by_join t key_src key_tgts)
  (*
     let by_join_map t key_src key_tgts f = by_join t ~f key_src key_tgts

     let by_join_map_u t key_src key_tgts f =
       Lwt.async (fun () -> by_join t ~f key_src key_tgts) *)

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

  let by_map2 t key_tgt key_src1 key_src2 f : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in
    let cb = real_push t key_tgt in

    Lwt_stream.iter_s
      (fun (v1, v2) ->
        cb (f (v1, v2)) ;
        Lwt.return_unit)
      (product_stream stream_src1 stream_src2)

  let by_map2_u t key_tgt key_src1 key_src2 f : unit =
    Lwt.async (fun () -> by_map2 t key_tgt key_src1 key_src2 f)

  let by_filter_map2 t key_tgt key_src1 key_src2 f : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in
    let cb = real_push t key_tgt in

    Lwt_stream.iter_s
      (fun (v1, v2) ->
        (match f (v1, v2) with Some v -> cb v | None -> ()) ;
        Lwt.return_unit)
      (product_stream stream_src1 stream_src2)

  let by_filter_map2_u t key_tgt key_src1 key_src2 f : unit =
    Lwt.async (fun () -> by_filter_map2 t key_tgt key_src1 key_src2 f)

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

  (* let by_join_both t key_tgt key_src_pairs f : unit Lwt.t =
     let srcs =
       List.map key_src_pairs ~f:(fun (a, b) ->
           let s1, s2 = (get_stream t a, get_stream t b) in
           product_stream s1 s2)
     in
     let cb = real_push t key_tgt in
     Lwt_list.iter_s
       (Lwt_stream.iter_s (fun v ->
            cb (f v) ;
            Lwt.return_unit))
       srcs *)

  (* external use *)
  let current_messages t key =
    Hashtbl.find_and_call t.map key
      ~if_found:(fun x -> x.messages)
      ~if_not_found:(fun _ -> [])
end
