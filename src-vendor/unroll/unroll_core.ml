open Core
open Lwt.Infix
include Unroll_intf
open Stream_helper
module N = Naive_state_machine

module Low_level (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  type key = Key.t
  type message = M.message

  type detail = {
    stream : message Lwt_stream.t;
    push : message option -> unit;
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
  let push_mutex = Nano_mutex.create ()

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

  let find_detail t key =
    Hashtbl.find_or_add t.map key ~default:(empty_detail t)

  let add_detail t key =
    let detail = empty_detail t () in
    Hashtbl.add_exn t.map ~key ~data:detail ;
    detail

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

  let stream_of_detail detail = Lwt_stream.clone detail.stream

  let get_push (detail : detail) msg =
    Nano_mutex.critical_section push_mutex ~f:(fun () ->
        if detail.is_dedup
           && List.mem detail.messages msg ~equal:M.equal_message
        then ()
        else
          (* Note this push is not for the current src-tgt pair.
             The handler function belonging to this target key is defined at the place
             where `push_msg_by_key` is used.
             This push is used for other pairs in which this target is their's source.
             That's the reason why we cannot see any handler here.
          *)
          match detail.pre_push msg with
          | Some msg ->
              detail.push (Some msg) ;
              detail.messages <- detail.messages @ [ msg ]
          | None -> ())

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
end

module User_level_common
    (L : Low_level)
    (M : M_sig with type message = L.message) =
struct
  let get_payload_stream u p =
    L.get_stream u p |> Lwt_stream.filter_map M.prj_opt

  let bg x = Lwt.async (fun () -> x >>= fun _ -> Lwt.return_unit)

  (* let push_opt t key p_opt =
     match p_opt with Some p -> L.push_msg_by_key t key p | None -> L.close t key *)

  let push t key p_opt =
    match p_opt with
    | Some p -> L.push_msg_by_key t key (M.inj p)
    | None -> L.close t key

  let then_lwt f x =
    f x ;
    Lwt.return_unit

  let push_payload u pipe p = L.push_msg u pipe (M.inj p)
end

module Payload_as_message (P : P_sig) = struct
  include P

  type message = payload

  let equal_message = equal_payload
  let inj x = x
  let prj_opt x = Some x
  let seq f1 f2 x = x |> f1 |> f2
  let seq_opt f_opt push x = match f_opt x with Some v -> push v | _ -> ()
  let seq2 = seq
  let seq2_opt = seq_opt
  let fmap f _ = f
end

module Payload_to_message (P : P_sig) = struct
  module M = struct
    type payload = P.payload
    type control = unit
    type message = Payload of payload | Control of control

    let equal_message m1 m2 =
      match (m1, m2) with
      | Payload p1, Payload p2 -> P.equal_payload p1 p2
      | Control _, Control _ -> true
      | _, _ -> false
  end

  include M

  let _not_here : message = Control ()
  let map_out f = Option.value_map ~default:() ~f
  let inj p = Payload p
  let prj_opt = function Payload p -> Some p | Control _ -> None

  (* all the folling function is human-craft due to the lack of `pair.fmap`, `tuple3.fmap` ... *)

  let fmap f d msg = match msg with Payload p -> f p | Control _ -> d

  let fmap2 f d (msg1, msg2) =
    match (msg1, msg2) with Payload p1, Payload p2 -> f (p1, p2) | _, _ -> d

  let mk_injo fmap f msg =
    (* match msg with Payload p -> Some (Payload (f p)) | Control _ -> None *)
    (* let f' x = x |> f |> inj |> Option.some in *)
    let f' p = Some (inj (f p)) in
    fmap f' None msg

  let fmap_injo f = mk_injo fmap f
  let fmap2_injo f = mk_injo fmap2 f

  let mk_opt fmap f_opt msg =
    (* let f' p = match f_opt p with Some v -> Some (Payload v) | None -> None in *)
    let f' p = Option.map (f_opt p) ~f:inj in
    fmap f' None msg

  let fmap_opt f_opt = mk_opt fmap f_opt
  let fmap2_opt f_opt = mk_opt fmap2 f_opt

  let mk_seq f_injo f_payload f_out msg =
    msg |> f_injo f_payload |> map_out f_out

  let seq f push = mk_seq fmap_injo f push
  let seq2 f push = mk_seq fmap2_injo f push
  let seq_opt f_opt push = mk_seq fmap_opt f_opt push
  let seq2_opt f_opt push = mk_seq fmap2_opt f_opt push
end

module Payload_to_U (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Message = Payload_to_message (P)
  module L = Low_level (Key) (Message)
  open Message
  include L

  type message = Message.message
  type pipe = detail
  type 'a act = 'a Lwt.t
  type payload = P.payload

  include User_level_common (L) (Message)

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(push_payload u pipe_dst) ;
    pipe_dst.push None ;
    Lwt.return pipe_dst

  let map u pipe_src dst f =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_detail
    |> Lwt_stream.iter_s (seq f (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  let filter_map u pipe_src dst f_opt =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_detail
    |> Lwt_stream.iter_s (seq_opt f_opt (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  (* let filter u pipe_src dst f = filter_map u pipe_src dst (from_filter f) *)
  let id u pipe_src dst = map u pipe_src dst Fn.id

  let map2 u pipe_src1 pipe_src2 dst f =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_s (seq2 f (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12
    |> Lwt_stream.iter_s (seq2_opt f_opt (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  let join u pipe_srcs dst =
    let stream_srcs = List.map pipe_srcs ~f:stream_of_detail in
    let pipe_dst = find_detail u dst in
    Lwt_list.iter_p (Lwt_stream.iter (get_push pipe_dst)) stream_srcs ;%lwt
    Lwt.return pipe_dst

  let iter _u pipe_src f =
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s (fmap f () |> then_lwt)

  let _bind0 u pipe_src f =
    pipe_src |> stream_of_detail
    |> Lwt_stream.filter_map (fun msg ->
           let f_to_pipe p =
             let key_dst = f p in
             Some (add_detail u key_dst)
           in
           fmap f_to_pipe None msg)
  (* |> Lwt_stream.concat |> Lwt.return *)
end

module Payload_as_message_to_U (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Message = Payload_as_message (P)
  module L = Low_level (Key) (Message)
  include L

  type 'a act = 'a Lwt.t
  type payload = P.payload
  type pipe = detail

  open Message
  include User_level_common (L) (Message)

  let stream_of_pipe pipe = Lwt_stream.clone pipe.stream

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(fun v -> pipe_dst.push (Some v)) ;
    pipe_dst.push None ;
    Lwt.return pipe_dst

  let map u pipe_src dst f =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_pipe
    |> Lwt_stream.iter_s (seq f (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  let filter_map u pipe_src dst f_opt =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_pipe
    |> Lwt_stream.iter_s (seq_opt f_opt (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  (* let filter u pipe_src dst f = filter_map u pipe_src dst (from_filter f) *)
  let id u pipe_src dst = map u pipe_src dst Fn.id

  let map2 u pipe_src1 pipe_src2 dst f =
    let stream_src1 = stream_of_pipe pipe_src1 in
    let stream_src2 = stream_of_pipe pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_p (seq2 f (get_push pipe_dst) |> then_lwt) ;%lwt
    close u dst ;
    Lwt.return pipe_dst

  let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
    let stream_src1 = stream_of_pipe pipe_src1 in
    let stream_src2 = stream_of_pipe pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12
    |> Lwt_stream.iter_s (seq2_opt f_opt (get_push pipe_dst) |> then_lwt) ;%lwt
    Lwt.return pipe_dst

  let join u pipe_srcs dst =
    let stream_srcs = List.map pipe_srcs ~f:stream_of_pipe in
    let pipe_dst = find_detail u dst in
    Lwt_list.iter_p (Lwt_stream.iter (get_push pipe_dst)) stream_srcs ;%lwt
    Lwt.return pipe_dst

  let iter _u pipe_src f =
    pipe_src |> stream_of_pipe |> Lwt_stream.iter_s (fmap f () |> then_lwt)

  let _bind0 u pipe_src f =
    pipe_src |> stream_of_pipe
    |> Lwt_stream.filter_map (fun msg ->
           let f_to_pipe p =
             let key_dst = f p in
             Some (add_detail u key_dst)
           in
           fmap f_to_pipe None msg)
  (* |> Lwt_stream.concat |> Lwt.return *)
end

module Change_use_lwt_to_unit
    (L : Low_level)
    (M : M_sig with type message = L.message)
    (U : User_level
           with type t = L.t
            and type key = L.key
            and type 'a act = 'a Lwt.t) =
struct
  type payload = U.payload
  type pipe = U.pipe
  type 'a act = unit

  include User_level_common (L) (M)

  let one_shot t key_src v = U.one_shot t key_src v |> bg
  let iter t key_src f = U.iter t key_src f |> bg
  let id t key_src key_dst = U.id t key_src key_dst |> bg
  let map t key_src key_dst f = U.map t key_src key_dst f |> bg
  let filter_map t key_src key_dst f = U.filter_map t key_src key_dst f |> bg
  let join t key_srcs key_dst = U.join t key_srcs key_dst |> bg

  let map2 t key_src1 key_src2 key_dst f =
    U.map2 t key_src1 key_src2 key_dst f |> bg

  let filter_map2 t key_src1 key_src2 key_dst f =
    U.filter_map2 t key_src1 key_src2 key_dst f |> bg
end

module Change_use_pipe_to_key_bg
    (L : Low_level)
    (M : M_sig with type message = L.message)
    (U : User_level
           with type t = L.t
            and type pipe = L.detail
            and type key = L.key
            and type payload = L.message
            and type 'a act = 'a Lwt.t) =
struct
  type payload = U.payload
  type key = L.key
  type pipe = L.key
  type 'a act = unit

  include User_level_common (L) (M)

  let to_pipe t key = L.find_detail t key
  let one_shot t key_src v = U.one_shot t key_src v |> bg
  let iter t key_src f = U.iter t (to_pipe t key_src) f |> bg
  let id t key_src key_dst = U.id t (to_pipe t key_src) key_dst |> bg
  let map t key_src key_dst f = U.map t (to_pipe t key_src) key_dst f |> bg

  let filter_map t key_src key_dst f =
    U.filter_map t (to_pipe t key_src) key_dst f |> bg

  let join t key_srcs key_dst =
    U.join t (List.map ~f:(to_pipe t) key_srcs) key_dst |> bg

  let map2 t key_src1 key_src2 key_dst f =
    U.map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f |> bg

  let filter_map2 t key_src1 key_src2 key_dst f =
    U.filter_map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f |> bg
end

module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Fg = Payload_as_message_to_U (Key) (P)
  include Fg

  module Bg = struct
    include Fg.L
    include Change_use_lwt_to_unit (Fg.L) (Fg.Message) (Fg)
  end
end

module Make_use_key (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Fg = Payload_as_message_to_U (Key) (P)
  include Fg.L
  include Change_use_pipe_to_key_bg (Fg.L) (Fg.Message) (Fg)
end

module Make_dummy_control (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Fg = Payload_to_U (Key) (P)
  include Fg

  module Bg = struct
    include Fg.L
    include Change_use_lwt_to_unit (Fg.L) (Fg.Message) (Fg)
  end
end
