open Core
open Lwt.Infix
include Unroll_intf

let then_lwt f x =
  f x ;
  Lwt.return_unit

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

(* let from_filter f msg = if f msg then Some msg else None *)
(* let to_filter f msg = Option.is_some (f msg) *)

let map_out f = Option.value_map ~default:() ~f

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

(* let join_map t key_dst key_srcs f = join t ~f key_dst key_srcs

   let join_map_u t key_dst key_srcs f =
     Lwt.async (fun () -> join t ~f key_dst key_srcs) *)

module N = Naive_state_machine

module Part_common (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  type key = Key.t
  type message = M.message

  type detail = {
    stream : message Lwt_stream.t;
    push : message option -> unit;
    mutable status : N.t;
    mutable pre_push : message -> message option;
    mutable messages : message list;
  }

  type t = { map : (Key.t, detail) Hashtbl.t }

  let msg_queue = ref []

  (* let add_msg msg =
     msg_queue := msg :: !msg_queue ;
     msg *)

  let create () = { map = Hashtbl.create (module Key) }
  let reset state = Hashtbl.clear state.map
  let push_mutex = Nano_mutex.create ()

  let empty_detail () =
    let stream, push = Lwt_stream.create () in
    let pre_push m = Some m in
    { stream; push; status = Initial; pre_push; messages = [] }

  let find_detail t key = Hashtbl.find_or_add t.map key ~default:empty_detail
  (* let stream_of_pipe detail = Lwt_stream.clone detail.stream *)

  let add_detail t key =
    let detail = empty_detail () in
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

  let get_push detail msg =
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

  let real_push t key =
    let detail = find_detail t key in
    get_push detail

  let real_close t key =
    let detail = find_detail t key in
    detail.push None

  let just_push t key v =
    match v with Some msg -> real_push t key msg | None -> real_close t key

  let push_all t key msgs = List.iter msgs ~f:(real_push t key)

  (* public *)

  (* external use *)
  let messages_sent t key =
    Hashtbl.find_and_call t.map key
      ~if_found:(fun x -> x.messages)
      ~if_not_found:(fun _ -> [])
end

module Part_use_unit (C : Common) (Key : Base.Hashtbl.Key.S) (M : M_sig) =
struct
  (* module C = Part_common (Key) (M) *)
  open C

  type payload = M.message
  type pipe = Key.t
  type 'a act = unit Lwt.t

  let seq f push x = x |> f |> push
  let map_in f push x = match f x with Some v -> push v | None -> ()

  let one_shot t key v =
    real_push t key v ;
    real_close t key ;
    Lwt.return_unit

  let id t key_src key_dst : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (real_push t key_dst |> then_lwt) stream_src

  let map t key_src key_dst f : unit Lwt.t =
    let stream_src = get_stream t key_src in
    (* (seq_opt f_opt push |> then_lwt) *)
    Lwt_stream.iter_s (seq f (real_push t key_dst) |> then_lwt) stream_src

  let filter_map t key_src key_dst f : unit Lwt.t =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s (map_in f (real_push t key_dst) |> then_lwt) stream_src

  let join t key_srcs key_dst =
    let stream_srcs = List.map key_srcs ~f:(get_stream t) in
    Lwt_list.iter_p (Lwt_stream.iter (real_push t key_dst)) stream_srcs

  let map2 t key_src1 key_src2 key_dst f : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in

    (* seq2 *)
    Lwt_stream.iter_s
      (seq f (real_push t key_dst) |> then_lwt)
      (product_stream stream_src1 stream_src2)

  let filter_map2 t key_src1 key_src2 key_dst f : unit Lwt.t =
    let stream_src1 = get_stream t key_src1 in
    let stream_src2 = get_stream t key_src2 in

    (* seq2_opt *)
    Lwt_stream.iter_s
      (map_in f (real_push t key_dst) |> then_lwt)
      (product_stream stream_src1 stream_src2)

  let iter t key_src f =
    let stream_src = get_stream t key_src in
    Lwt_stream.iter_s f stream_src
end

(* The following four modules are used to derive another Use by just changing a
   _foreground_ Lwt to a _background_ unit.
   I don't figure out a correct way to merge `Change_use_lwt_unit_to_unit` and
   `Change_use_lwt_unit_to_unit` into a functor.
*)

module L_lwt_unit_to_unit = struct
  type 'b t_in = unit Lwt.t
  type 'c t_out = unit

  let lift1 f a1 = Lwt.async (fun () -> f a1)
  let lift2 f a1 a2 = Lwt.async (fun () -> f a1 a2)
  let lift3 f a1 a2 a3 = Lwt.async (fun () -> f a1 a2 a3)
  let lift4 f a1 a2 a3 a4 = Lwt.async (fun () -> f a1 a2 a3 a4)
  let lift5 f a1 a2 a3 a4 a5 = Lwt.async (fun () -> f a1 a2 a3 a4 a5)
end

module L_lwt_to_unit = struct
  type 'b t_in = 'b Lwt.t
  type 'c t_out = unit

  open Lwt.Infix

  let lift1 f a1 = Lwt.async (fun () -> f a1 >>= fun _ -> Lwt.return_unit)
  let lift2 f a1 a2 = Lwt.async (fun () -> f a1 a2 >>= fun _ -> Lwt.return_unit)

  let lift3 f a1 a2 a3 =
    Lwt.async (fun () -> f a1 a2 a3 >>= fun _ -> Lwt.return_unit)

  let lift4 f a1 a2 a3 a4 =
    Lwt.async (fun () -> f a1 a2 a3 a4 >>= fun _ -> Lwt.return_unit)

  let lift5 f a1 a2 a3 a4 a5 =
    Lwt.async (fun () -> f a1 a2 a3 a4 a5 >>= fun _ -> Lwt.return_unit)
end

module Change_use_lwt_unit_to_unit
    (U : Use with type 'a act = unit Lwt.t)
    (L : Lifter with type 'b t_in = unit Lwt.t and type 'c t_out = unit) =
struct
  type payload = U.payload
  type pipe = U.pipe
  type 'a act = unit

  let one_shot t key_src v = L.lift3 U.one_shot t key_src v
  let iter t key_src f = L.lift3 U.iter t key_src (then_lwt f)
  let id t key_src key_dst = L.lift3 U.id t key_src key_dst
  let map t key_src key_dst f = L.lift4 U.map t key_src key_dst f
  let filter_map t key_src key_dst f = L.lift4 U.filter_map t key_src key_dst f
  let join t key_srcs key_dst = L.lift3 U.join t key_srcs key_dst

  let map2 t key_src1 key_src2 key_dst f =
    L.lift5 U.map2 t key_src1 key_src2 key_dst f

  let filter_map2 t key_src1 key_src2 key_dst f =
    L.lift5 U.filter_map2 t key_src1 key_src2 key_dst f
end

module Change_use_lwt_to_unit
    (U : Use with type 'a act = 'a Lwt.t)
    (L : Lifter with type 'b t_in = 'b Lwt.t and type 'c t_out = unit) =
struct
  type payload = U.payload
  type pipe = U.pipe
  type 'a act = unit

  let one_shot t key_src v = L.lift3 U.one_shot t key_src v
  let iter t key_src f = L.lift3 U.iter t key_src (then_lwt f)
  let id t key_src key_dst = L.lift3 U.id t key_src key_dst
  let map t key_src key_dst f = L.lift4 U.map t key_src key_dst f
  let filter_map t key_src key_dst f = L.lift4 U.filter_map t key_src key_dst f
  let join t key_srcs key_dst = L.lift3 U.join t key_srcs key_dst

  let map2 t key_src1 key_src2 key_dst f =
    L.lift5 U.map2 t key_src1 key_src2 key_dst f

  let filter_map2 t key_src1 key_src2 key_dst f =
    L.lift5 U.filter_map2 t key_src1 key_src2 key_dst f
end

(* U.iter is the old iter which iterates `f` on `key_src`
   This function is to make a new iter which uses U.iter but with the modified return type.

   `f` is the worker function, `payload -> unit Lwt.t`
   `U.iter` will finally return `unit Lwt.t`.

   `iter` should return `unit` finally.
   Luckily with `L_lwt_to_unit.lift3`, it should be able to _lift_ `unit Lwt.t` to `unit`.
*)

module Make0 (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  module Common = Part_common (Key) (M)
  module Use = Part_use_unit (Common) (Key) (M)
end

module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
  module M0 = Make0 (Key) (M)
  include M0.Common
  include M0.Use
end

module Make_just_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module M0 =
    Make0
      (Key)
      (struct
        include P

        type message = payload

        let equal_message = equal_payload
      end)

  include M0.Common
  include M0.Use
end

module Make_just_payload_no_wait (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module M = struct
    include P

    type message = payload

    let equal_message = equal_payload
  end

  module M0 = Make0 (Key) (M)
  include M0.Common

  include
    Change_use_lwt_unit_to_unit
      (struct
        type nonrec t = t
        type nonrec key = key

        include M0.Use
      end)
      (L_lwt_unit_to_unit)
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
  let inj p = Payload p

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

module Make_pipe (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module Message = Payload_to_message (P)
  module Common = Part_common (Key) (Message)
  include Common

  type 'a act = 'a Lwt.t
  type payload = P.payload

  module Pipe = struct
    open Message

    type pipe = detail

    let stream_of_pipe pipe = Lwt_stream.clone pipe.stream

    let one_shot u dst v =
      let pipe_dst = add_detail u dst in
      pipe_dst.push (Some (inj v)) ;
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
      let stream12 = product_stream stream_src1 stream_src2 in
      let pipe_dst = add_detail u dst in
      stream12 |> Lwt_stream.iter_s (seq2 f (get_push pipe_dst) |> then_lwt) ;%lwt
      Lwt.return pipe_dst

    let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
      let stream_src1 = stream_of_pipe pipe_src1 in
      let stream_src2 = stream_of_pipe pipe_src2 in
      let stream12 = product_stream stream_src1 stream_src2 in
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
      pipe_src |> stream_of_pipe |> Lwt_stream.iter_s (fmap f Lwt.return_unit)

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

  include Pipe
end

module Make_pipe_no_wait (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
  module MP = Make_pipe (Key) (P)
  include MP
  include Change_use_lwt_to_unit (MP) (L_lwt_to_unit)
end
