open Core
open Lwt.Infix
include Unroll_intf
open Stream_helper
open Messages
module N = Naive_state_machine

module Low_level (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  type key = Key.t
  type message = M.message

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
  let push_mutex = Nano_mutex.create ()

  (* stream basic *)

  let empty_detail t () =
    let stream, push = Lwt_stream.create () in
    let pre_push m = Some m in
    {
      stream;
      push;
      status = Running;
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
        else ())

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
  let map_push f pipe = M.seq f (L.get_push pipe) |> then_lwt
  let map_opt_push f_opt pipe = M.seq_opt f_opt (L.get_push pipe) |> then_lwt
  let map2_opt_push f pipe = M.seq2 f (L.get_push pipe) |> then_lwt
  let map2_opt_opt_push f pipe = M.seq2_opt f (L.get_push pipe) |> then_lwt
  let push_state _u _ _ = ()
end

module Make_fg (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  module L = Low_level (Key) (M)
  include L

  type pipe = detail
  type payload = M.payload
  type 'a act = 'a Lwt.t

  include User_level_common (L) (M)

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(push_payload u pipe_dst) ;
    pipe_dst.push None ;
    Lwt.return pipe_dst

  let map u pipe_src dst f =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s (map_push f pipe_dst) ;%lwt
    Lwt.return pipe_dst

  let id u pipe_src dst = map u pipe_src dst Fn.id

  let filter_map u pipe_src dst f_opt =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_detail
    |> Lwt_stream.iter_s (map_opt_push f_opt pipe_dst) ;%lwt
    Lwt.return pipe_dst

  (* let filter u pipe_src dst f = filter_map u pipe_src dst (from_filter f) *)

  let map2 u pipe_src1 pipe_src2 dst f =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_s (map2_opt_push f pipe_dst) ;%lwt
    close u dst ;
    Lwt.return pipe_dst

  let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_s (map2_opt_opt_push f_opt pipe_dst) ;%lwt
    Lwt.return pipe_dst

  let join u pipe_srcs dst =
    let stream_srcs = List.map pipe_srcs ~f:stream_of_detail in
    let pipe_dst = find_detail u dst in
    Lwt_list.iter_p (Lwt_stream.iter (get_push pipe_dst)) stream_srcs ;%lwt
    Lwt.return pipe_dst

  let iter _u pipe_src f =
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s (M.fmap f () |> then_lwt)

  let bind_like _u _pipe_src _f _dst = failwith "not yet"

  let _bind0 u pipe_src f =
    pipe_src |> stream_of_detail
    |> Lwt_stream.filter_map (fun msg ->
           let f_to_pipe p =
             let key_dst = f p in
             Some (add_detail u key_dst)
           in
           M.fmap f_to_pipe None msg)
  (* |> Lwt_stream.concat |> Lwt.return *)
end

module User_level_to_bg
    (L : Low_level)
    (M : M_sig with type message = L.message)
    (U : User_level
           with type t = L.t
            and type key = L.key
            and type payload = M.payload
            and type 'a act = 'a Lwt.t) :
  User_level
    with type t = L.t
     and type key = L.key
     and type payload = M.payload
     and type pipe = U.pipe
     and type 'a act = unit = struct
  type t = L.t
  type key = L.key
  type payload = M.payload
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

  let bind_like _u _pipe_src _f _dst = failwith "not yet"
end

module User_level_to_key
    (L : Low_level)
    (M : M_sig with type message = L.message)
    (U : User_level
           with type t = L.t
            and type pipe = L.detail
            and type key = L.key
            and type payload = M.payload
            and type 'a act = 'a Lwt.t) :
  User_level
    with type t = L.t
     and type key = L.key
     and type pipe = L.key
     and type payload = M.payload
     and type 'a act = 'a Lwt.t = struct
  type t = L.t
  type payload = M.payload
  type key = L.key
  type pipe = L.key
  type 'a act = 'a Lwt.t

  include User_level_common (L) (M)

  let bg key _ = Lwt.return key
  let to_pipe t key = L.find_detail t key
  let one_shot t key_dst v = U.one_shot t key_dst v |> bg key_dst
  let iter t key_dst f = U.iter t (to_pipe t key_dst) f
  let id t key_src key_dst = U.id t (to_pipe t key_src) key_dst |> bg key_dst

  let map t key_src key_dst f =
    U.map t (to_pipe t key_src) key_dst f |> bg key_dst

  let filter_map t key_src key_dst f =
    U.filter_map t (to_pipe t key_src) key_dst f |> bg key_dst

  let join t key_srcs key_dst =
    U.join t (List.map ~f:(to_pipe t) key_srcs) key_dst |> bg key_dst

  let map2 t key_src1 key_src2 key_dst f =
    U.map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f |> bg key_dst

  let filter_map2 t key_src1 key_src2 key_dst f =
    U.filter_map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f
    |> bg key_dst

  let bind_like _u _pipe_src _f _dst = failwith "not yet"
end

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  module Fg = Make_fg (Key) (M)
  include Fg

  module Bg = struct
    include Fg.L
    include User_level_to_bg (Fg.L) (M) (Fg)
  end
end

(* derived *)
module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) =
  Make (Key) (Payload_as_message (P))

module Make_dummy_control (Key : Base.Hashtbl.Key.S) (P : P_sig) =
  Make (Key) (Payload_to_message (P))

module Make_use_key (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module M = Payload_as_message (P)
  module U = Make (Key) (M)
  include U.L

  (* include User_level_to_key_bg (U.L) (M) (U.Fg) *)
  module U_key_fg = User_level_to_key (U.L) (M) (U.Fg)
  include User_level_to_bg (U.L) (M) (U_key_fg)
end

module Make_control (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module M = struct
    type payload = P.payload
    type message = Payload of payload | Set_state of N.t

    let equal_message m1 m2 =
      match (m1, m2) with
      | Payload p1, Payload p2 -> P.equal_payload p1 p2
      | Set_state _, Set_state _ -> true
      | _, _ -> false

    let map_out f = Option.value_map ~default:() ~f
    let inj p = Payload p
    let prj_opt = function Payload p -> Some p | Set_state _ -> None
    let fmap f d msg = match msg with Payload p -> f p | Set_state _ -> d

    let fmap2 f d (msg1, msg2) =
      match (msg1, msg2) with Payload p1, Payload p2 -> f (p1, p2) | _, _ -> d

    let fmap_opt f_opt msg =
      msg |> fmap (fun p -> Option.map (f_opt p) ~f:inj) None

    let fmap2_opt f_opt msg =
      msg |> fmap2 (fun p -> Option.map (f_opt p) ~f:inj) None

    let seq f push msg =
      msg |> fmap (fun p -> Some (inj (f p))) None |> map_out push

    let seq2 f push msg =
      msg |> fmap2 (fun p -> Some (inj (f p))) None |> map_out push

    let seq_opt f_opt push msg = msg |> fmap_opt f_opt |> map_out push
    let seq2_opt f_opt push msg = msg |> fmap2_opt f_opt |> map_out push
  end

  module L = Low_level (Key) (M)
  include L

  type pipe = detail
  type payload = M.payload
  type 'a act = 'a Lwt.t

  (* include User_level_common (L) (M) *)
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

  let push t key p_opt =
    match p_opt with
    | Some p -> L.push_msg_by_key t key (M.inj p)
    | None -> L.close t key

  let then_lwt f x =
    f x ;
    Lwt.return_unit

  let push_payload u pipe p = L.push_msg u pipe (M.inj p)
  let map_push f pipe = M.seq f (L.get_push pipe) |> then_lwt
  let map_opt_push f_opt pipe = M.seq_opt f_opt (L.get_push pipe) |> then_lwt
  let map2_opt_push f pipe = M.seq2 f (L.get_push pipe) |> then_lwt
  let map2_opt_opt_push f pipe = M.seq2_opt f (L.get_push pipe) |> then_lwt

  let push_state u key state =
    let detail = find_detail u key in
    detail.status <- N.to_fail detail.status ;
    detail.push (Some (M.Set_state state)) ;
    detail.push None

  let one_shot u dst vs =
    let pipe_dst = add_detail u dst in
    List.iter vs ~f:(push_payload u pipe_dst) ;
    pipe_dst.push None ;
    Lwt.return pipe_dst

  let map u pipe_src dst f =
    let pipe_dst = add_detail u dst in
    let f_single_fate msg =
      if N.equal pipe_dst.status N.Running
      then
        match msg with
        | M.Set_state s' ->
            pipe_dst.status <- N.safe_transform pipe_dst.status s' ;
            (* Lwt.return_unit *)
            msg |> map_push f pipe_dst
        | _ -> msg |> map_push f pipe_dst
      else Lwt.return_unit
    in
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s f_single_fate ;%lwt
    Lwt.return pipe_dst

  let id u pipe_src dst = map u pipe_src dst Fn.id

  let filter_map u pipe_src dst f_opt =
    let pipe_dst = add_detail u dst in
    pipe_src |> stream_of_detail
    |> Lwt_stream.iter_s (map_opt_push f_opt pipe_dst) ;%lwt
    Lwt.return pipe_dst

  (* let filter u pipe_src dst f = filter_map u pipe_src dst (from_filter f) *)

  let map2 u pipe_src1 pipe_src2 dst f =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_s (map2_opt_push f pipe_dst) ;%lwt
    close u dst ;
    Lwt.return pipe_dst

  let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
    let stream_src1 = stream_of_detail pipe_src1 in
    let stream_src2 = stream_of_detail pipe_src2 in
    let stream12 = product_stream_bg stream_src1 stream_src2 in
    let pipe_dst = add_detail u dst in
    stream12 |> Lwt_stream.iter_s (map2_opt_opt_push f_opt pipe_dst) ;%lwt
    Lwt.return pipe_dst

  let join u pipe_srcs dst =
    let stream_srcs = List.map pipe_srcs ~f:stream_of_detail in
    let pipe_dst = add_detail u dst in
    Lwt_list.iter_p (Lwt_stream.iter (get_push pipe_dst)) stream_srcs ;%lwt
    Lwt.return pipe_dst

  let iter _u pipe_src f =
    pipe_src |> stream_of_detail |> Lwt_stream.iter_s (M.fmap f () |> then_lwt)

  let bind_like u pipe_src f dst =
    let pipe_src = stream_of_detail pipe_src in
    let pipe_dst = add_detail u dst in
    let f_here p =
      let key_src2 = f p in
      let pipe_src2 = find_detail u key_src2 in
      Lwt_stream.iter_s
        (M.fmap (fun p -> push u dst (Some p)) () |> then_lwt)
        (stream_of_detail pipe_src2)
    in

    let f_iter msg = (M.fmap f_here Lwt.return_unit) msg in
    Lwt_stream.iter_p f_iter pipe_src ;%lwt
    Lwt.return pipe_dst

  let _bind0 u pipe_src f =
    pipe_src |> stream_of_detail
    |> Lwt_stream.filter_map (fun msg ->
           let f_to_pipe p =
             let key_dst = f p in
             Some (add_detail u key_dst)
           in
           M.fmap f_to_pipe None msg)
  (* |> Lwt_stream.concat |> Lwt.return *)
end

module Make_control_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module U = Make_control (Key) (P)
  include U.L
  module U_key = User_level_to_key (U.L) (U.M) (U)
  include User_level_to_bg (U.L) (U.M) (U_key)
end
