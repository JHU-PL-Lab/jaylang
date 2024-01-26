open Core
open Lwt.Infix
include Unroll_intf
open Stream_helper
open Messages
module N = Naive_state_machine

module Low_level (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  type key = Key.t
  type message = M.message

  (* This structure (a pair of push and stream) naturally supports multiple producer of a stream, as long as
      it can get the `push` function.
      However, to support this, a detail should keep track of all who holds `push`.
      It may not be suitable for this application since every time when a key-stream pair is created, the possible
     `push`-ing way is fixed
  *)
  type detail = {
    push : message option -> unit;
    stream : message Lwt_stream.t;
    mutable status : N.t;
    mutable pre_push : message -> message option;
    mutable messages : message list;
    is_dedup : bool;
  }

  type t = { map : (Key.t, detail) Hashtbl.t; is_dedup : bool }

  let msg_queue = ref []

  (* let add_msg msg =
     msg_queue := msg :: !msg_queue ;
     msg *)

  let create ?(is_dedup = true) () =
    { map = Hashtbl.create (module Key); is_dedup }

  let reset state = Hashtbl.clear state.map
  let _push_mutex = Nano_mutex.create ()

  (* stream basic *)

  let empty_detail t () =
    let stream, push = Lwt_stream.create () in
    let pre_push m = Some m in
    {
      stream;
      push;
      status = Initial;
      pre_push;
      messages = [];
      is_dedup = t.is_dedup;
    }

  (* find or create a detail to listen *)
  let find_detail t key =
    Hashtbl.find_or_add t.map key ~default:(empty_detail t)

  let add_detail t key =
    let detail = find_detail t key in
    detail.status <- N.safe_transform detail.status N.Running ;
    detail

  let set_creation detail =
    detail.status <- N.safe_transform detail.status N.Running

  (* TODO: buggy warning *)
  let create_key t ?task key =
    let detail = find_detail t key in
    if N.is_status_initial detail.status
    then
      (* detail.status <- Running ; *)
      match task with None -> () | Some t -> t ()
    else ()

  let get_status t key =
    let detail = find_detail t key in
    detail.status

  let set_pre_push t key pre_push =
    let detail = find_detail t key in
    detail.pre_push <- pre_push

  (* low-level *)

  let get_stream t key =
    let detail = find_detail t key in
    Lwt_stream.clone detail.stream

  let stream_of_detail detail = Lwt_stream.clone detail.stream

  let stream_of_key u src =
    let pipe_src = find_detail u src in
    let stream_src = stream_of_detail pipe_src in
    stream_src

  (* TODO: try removing the use of `bg` *)
  (* let bind_like ?sp src1 f_opt =
     let stream, push =
       match sp with
       | Some (stream, push) -> (stream, push)
       | None -> Lwt_stream.create ()
     in
     Lwt_stream.iter_p
       (fun v1 ->
         match f_opt v1 with
         | Some src2 -> Lwt_stream.iter (fun v2 -> push (Some v2)) src2
         | None -> Lwt.return_unit)
       src1
     >|= (fun () -> push None)
     |> bg_magic ;
     stream *)

  (* let get_push (detail : detail) msg =
     Nano_mutex.critical_section push_mutex ~f:(fun () ->
         if detail.is_dedup
            && List.mem detail.messages msg ~equal:M.equal_message
         then ()
         else if (* Note this push is not for the current src-tgt pair.
                    The handler function belonging to this target key is defined at the place
                    where `push_msg_by_key` is used.
                    This push is used for other pairs in which this target is their's source.
                    That's the reason why we cannot see any handler here.
                 *)
                 N.equal detail.status N.Running
         then
           match detail.pre_push msg with
           | Some msg ->
               detail.push (Some msg) ;
               detail.messages <- detail.messages @ [ msg ]
           | None -> ()
         else ()) *)

  let get_push (detail : detail) msg =
    if detail.is_dedup && List.mem detail.messages msg ~equal:M.equal_message
    then ()
    else if (* Note this push is not for the current src-tgt pair.
               The handler function belonging to this target key is defined at the place
               where `push_msg_by_key` is used.
               This push is used for other pairs in which this target is their's source.
               That's the reason why we cannot see any handler here.
            *)
            (* TODO: buggy *)
            (not (N.equal detail.status N.Fail))
            || not (N.equal detail.status N.Done)
            (* true *)
    then
      match detail.pre_push msg with
      | Some msg ->
          detail.push (Some msg) ;
          detail.messages <- detail.messages @ [ msg ]
      | None -> ()
    else ()

  (* push basic *)

  let push_msg_by_key t key =
    let detail = find_detail t key in
    get_push detail

  let push_msg _t detail msg = get_push detail msg

  let close t key =
    let detail = find_detail t key in
    detail.push None

  (* external use *)
  let messages_sent t key =
    Hashtbl.find_and_call t.map key
      ~if_found:(fun x -> x.messages)
      ~if_not_found:(fun _ -> [])

  let dump t = Fmt.pr "len=%d@." (Hashtbl.length t.map)
end

module User_level_common
    (L : Low_level)
    (M : M_sig with type message = L.message) =
struct
  let get_payload_stream u k =
    L.get_stream u k |> Lwt_stream.filter_map M.prj_opt

  let get_payloads u k = get_payload_stream u k |> Lwt_stream.to_list

  let get_available_payloads u k =
    get_payload_stream u k |> Lwt_stream.get_available

  let set_pre_push_payload u k f =
    let f' p : M.message option =
      let p_opt = M.prj_opt p in
      Option.map p_opt ~f:(fun p -> Option.map (f p) ~f:M.inj) |> Option.join
    in
    L.set_pre_push u k f'

  let bg x = Lwt.async (fun () -> x >>= fun _ -> Lwt.return_unit)

  let push t key p_opt =
    match p_opt with
    | Some p -> L.push_msg_by_key t key (M.inj p)
    | None -> L.close t key

  let then_lwt f x =
    f x ;
    Lwt.return_unit

  let push_payload u pipe p = L.push_msg u pipe (M.inj p)
  let push_state _u _ _ = ()

  let product_pipe u src1 src2 =
    product_stream_bg (L.stream_of_key u src1) (L.stream_of_key u src2)

  let filter_payload_pair_opt f_opt (msg1, msg2) =
    (* match (msg1, msg2) with
       | Payload p1, Payload p2 -> (
           match f_opt (p1, p2) with Some p -> Some (Payload p) | None -> None)
       | _, _ -> None *)
    M.fmap2 f_opt None (msg1, msg2) |> Option.map ~f:M.inj

  let filter_payload_pair f (msg1, msg2) =
    (* match (msg1, msg2) with
       | Payload p1, Payload p2 -> Some (Payload (f (p1, p2)))
       | _, _ -> None *)
    M.fmap2 (fun p -> Some (M.inj (f p))) None (msg1, msg2)

  let filteri_payload_opt f (i, msg) =
    (* match msg with Payload p -> Some (Payload (f (i, p))) | _ -> None *)
    M.fmap (fun p -> Some (M.inj (f (i, p)))) None msg

  (* let iter_push u stream_src dst f =
     let pipe_dst = L.add_detail u dst in
     Lwt_stream.iter_s f stream_src >|= fun () -> L.close u dst *)

  let iter_push u stream_src dst =
    let f_here = L.push_msg_by_key u dst |> then_lwt in
    Lwt_stream.iter_s f_here stream_src >|= (fun () -> L.close u dst) |> bg

  let map u src f = L.stream_of_key u src |> Lwt_stream.map (M.map_payload f)

  let set u stream_src key_dst =
    iter_push u stream_src key_dst ;
    L.set_creation (L.find_detail u key_dst)
end

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  module L = Low_level (Key) (M)
  include L

  type payload = M.payload
  type 'a act = unit

  include User_level_common (L) (M)

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(push_payload u pipe_dst) ;
    pipe_dst.push None

  let set u stream_src dst =
    let pipe_dst = add_detail u dst in
    Lwt_stream.iter_s (L.get_push pipe_dst |> then_lwt) stream_src
    >|= (fun () -> close u dst)
    |> bg

  let iter0 _u stream_src f = Lwt_stream.iter_s f stream_src

  let iter_list_push u stream_srcs dst f =
    let pipe_dst = find_detail u dst in
    Lwt_list.iter_p (Lwt_stream.iter f) stream_srcs >|= fun () -> close u dst

  let id u src dst =
    let stream_src =
      stream_of_key u src |> Lwt_stream.map (M.map_payload Fn.id)
    in
    set u stream_src dst

  let filter_map u src dst f_opt =
    let stream_src =
      stream_of_key u src |> Lwt_stream.filter_map (M.filter_map_payload f_opt)
    in
    set u stream_src dst

  (* let filter u pipe_src dst f = filter_map u pipe_src dst (from_filter f) *)

  let map2 u src1 src2 dst f =
    let stream_src =
      product_pipe u src1 src2 |> Lwt_stream.filter_map (filter_payload_pair f)
    in
    set u stream_src dst

  let filter_map2 u src1 src2 dst f_opt =
    let stream_src =
      product_pipe u src1 src2
      |> Lwt_stream.filter_map (filter_payload_pair_opt f_opt)
    in
    set u stream_src dst

  let join u srcs dst =
    let stream_srcs =
      List.map srcs ~f:(find_detail u) |> List.map ~f:stream_of_detail
    in
    let pipe_dst = find_detail u dst in
    iter_list_push u stream_srcs dst (get_push pipe_dst) |> bg

  let iter u src f =
    iter0 u (stream_of_key u src) (M.fmap f () |> then_lwt) |> bg

  let bind_like u src dst f_to_key =
    let pipe_dst = add_detail u dst in
    let f_to_stream_push p =
      match f_to_key p with
      | Some key_src2 ->
          let pipe_src2 = find_detail u key_src2 in
          Lwt_stream.iter_s
            (M.fmap (fun p -> push u dst (Some p)) () |> then_lwt)
            (stream_of_detail pipe_src2)
      | None -> Lwt.return_unit
    in

    let on_done =
      Lwt_stream.iter_p
        (M.fmap f_to_stream_push Lwt.return_unit)
        (stream_of_key u src)
    in
    on_done |> bg

  let bind_like_list _u _src _dst _f = failwith "bind_like_list"
  let on _u _src _status_on _f _dst = failwith "on"
  let joini _u _srcs _dst _f = failwith "joini"
end

module Make_just_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) =
  Make (Key) (Payload_as_message (P))

module Make_dummy_control (Key : Base.Hashtbl.Key.S) (P : P_sig) =
  Make (Key) (Payload_to_message (P))

module Make_stateful (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module M = Payload_to_message (P)
  module L = Low_level (Key) (M)
  include L

  type payload = M.payload
  type 'a act = unit

  include User_level_common (L) (M)

  let push_state u key status' =
    let detail = find_detail u key in
    detail.status <- N.safe_transform detail.status status' ;
    detail.push (Some (M.Set_state status')) ;

    match status' with
    | N.Fail -> detail.push None
    | N.Done -> detail.push None
    | _ -> ()

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(push_payload u pipe_dst) ;
    push_state u dst N.Done
  (* pipe_dst.push None *)

  (* iter on pipe with payload *)
  let iter0_msg _u pipe_src f =
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s f

  (* iter on key with message *)
  let iter_msg u src f =
    let pipe_src = find_detail u src in
    iter0_msg u pipe_src f

  (* let iter0 u pipe_src f = iter0_msg u pipe_src (M.fmap f () |> then_lwt) *)
  let iter u src f = iter_msg u src (M.fmap f () |> then_lwt) |> bg

  let product_pipe u src1 src2 =
    product_stream_bg (stream_of_key u src1) (stream_of_key u src2)

  (* let map u src dst f =
     let stream_src = stream_of_key u src |> Lwt_stream.map (M.map_payload f) in
     set u stream_src dst *)

  let id u src dst = set u (map u src Fn.id) dst

  let filter_map u src dst f_opt =
    let stream_src =
      stream_of_key u src |> Lwt_stream.filter_map (M.filter_map_payload f_opt)
    in
    set u stream_src dst

  (* TODO: pointless to handle state in map2 *)
  let map2 u src1 src2 dst f =
    let stream_src =
      product_pipe u src1 src2 |> Lwt_stream.filter_map (filter_payload_pair f)
    in
    set u stream_src dst

  let filter_map2 u src1 src2 dst f_opt =
    let stream_src =
      product_pipe u src1 src2
      |> Lwt_stream.filter_map (filter_payload_pair_opt f_opt)
    in
    set u stream_src dst

  let join u srcs dst =
    let stream_src =
      let pipe_srcs = List.map srcs ~f:(find_detail u) in
      let stream_srcs = List.map pipe_srcs ~f:stream_of_detail in
      Lwt_stream.choose stream_srcs
    in
    set u stream_src dst

  let joini u srcs dst f =
    let stream_src =
      let pipe_srcs = List.map srcs ~f:(find_detail u) in
      let stream_srcs = List.map pipe_srcs ~f:stream_of_detail in
      let streami_srcs =
        List.mapi stream_srcs ~f:(fun i s ->
            Lwt_stream.map (fun msg -> (i, msg)) s)
      in
      Lwt_stream.choose streami_srcs
      |> Lwt_stream.filter_map (filteri_payload_opt f)
    in
    set u stream_src dst

  let bind_like u src dst f_to_key =
    let pipe_dst = find_detail u dst in
    let f_to_stream m =
      match M.prj_opt m with
      | Some p -> (
          match f_to_key p with
          | Some k -> k |> find_detail u |> stream_of_detail |> Option.some
          | None -> None)
      | None -> None
    in
    let stream_msg =
      bind0
        ~sp:(pipe_dst.stream, pipe_dst.push)
        (stream_of_key u src) f_to_stream
    in
    set_creation pipe_dst

  let bind_like_list_r u src dst f =
    let pipe_src = find_detail u src in
    let pipe_src = stream_of_detail pipe_src in
    let pipe_dst = find_detail u dst in
    let f_here p =
      let key_srcs = f p in
      let key_src2 = List.hd_exn key_srcs in
      let pipe_src2 = find_detail u key_src2 in
      Lwt_stream.iter_s
        (M.fmap (fun p -> push u dst (Some p)) () |> then_lwt)
        (stream_of_detail pipe_src2)
    in

    let f_iter msg = (M.fmap f_here Lwt.return_unit) msg in
    Lwt_stream.iter_p f_iter pipe_src |> bg ;
    pipe_dst

  let bind_like_list u srcs dst f =
    let pipe_dst = bind_like_list_r u srcs dst f in
    set_creation pipe_dst

  let on_r u src_status status_on src_v dst =
    let pipe_src = find_detail u src_status in
    let pipe_src = stream_of_detail pipe_src in
    let pipe_dst = find_detail u dst in
    let f_here () =
      let pipe_src2 = find_detail u src_v in
      Lwt_stream.iter_s
        (M.fmap (fun p -> push u dst (Some p)) () |> then_lwt)
        (stream_of_detail pipe_src2)
    in
    let f_iter msg =
      match msg with
      | M.Payload _p -> Lwt.return_unit
      | M.Set_state s when N.equal s status_on ->
          f_here () ;%lwt
          Lwt.return_unit
      | M.Set_state _ -> Lwt.return_unit
    in
    Lwt_stream.iter_s f_iter pipe_src |> bg ;
    pipe_dst

  let on u src_status status_on src_v dst =
    let pipe_dst = on_r u src_status status_on src_v dst in
    set_creation pipe_dst
end
