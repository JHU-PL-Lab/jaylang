(*

   module Make_use_key (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
     module Fg = Payload_as_message_to_U (Key) (P)
     include Fg.L

     (* include User_level_use_key_to_bg (Fg.L) (Fg.Message) (Fg) *)
     module U_key = User_level_use_key (Fg.L) (Fg.Message) (Fg)
     include User_level_to_bg (Fg.L) (Fg.Message) (U_key)
   end
*)

(*

   module User_level_use_key
       (L : Low_level)
       (M : M_sig with type message = L.message and type payload = L.message)
       (U : User_level
              with type t = L.t
               and type key = L.key
               and type pipe = L.detail
               and type payload = L.message
               and type 'a act = 'a Lwt.t) :
     User_level
       with type t = L.t
        and type key = L.key
        and type pipe = L.key
        and type payload = M.payload
        and type 'a act = 'a Lwt.t = struct
     type t = L.t
     type key = L.key
     type pipe = L.key
     type payload = M.payload
     type 'a act = 'a Lwt.t

     include User_level_common (L) (M)
     open Lwt.Infix

     let to_pipe t key = L.find_detail t key
     let one_shot t key_src v = U.one_shot t key_src v >|= fun _ -> key_src
     let iter t key_src f = U.iter t (to_pipe t key_src) f

     let id t key_src key_dst =
       U.id t (to_pipe t key_src) key_dst >|= fun _ -> key_dst

     let map t key_src key_dst f =
       U.map t (to_pipe t key_src) key_dst f >|= fun _ -> key_dst

     let filter_map t key_src key_dst f =
       U.filter_map t (to_pipe t key_src) key_dst f >|= fun _ -> key_dst

     let join t key_srcs key_dst =
       U.join t (List.map ~f:(to_pipe t) key_srcs) key_dst >|= fun _ -> key_dst

     let map2 t key_src1 key_src2 key_dst f =
       U.map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f >|= fun _ ->
       key_dst

     let filter_map2 t key_src1 key_src2 key_dst f =
       U.filter_map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f
       >|= fun _ -> key_dst
   end
*)

(*

   module Payload_as_message_to_U (Key : Base.Hashtbl.Key.S) (Message : M_sig) =
   struct
     module Message = Message
     open Message
     module L = Low_level (Key) (Message)
     include L

     type 'a act = 'a Lwt.t
     type payload = Message.payload
     type pipe = detail

     include User_level_common (L) (Message)

     let stream_of_pipe pipe = Lwt_stream.clone pipe.stream

     let one_shot u dst vs =
       let pipe_dst = add_detail u dst in
       List.iter vs ~f:(push_payload u pipe_dst) ;
       pipe_dst.push None ;
       Lwt.return pipe_dst

     let map u pipe_src dst f =
       let pipe_dst = add_detail u dst in
       pipe_src |> stream_of_pipe |> Lwt_stream.iter_s (map_push f pipe_dst) ;%lwt
       Lwt.return pipe_dst

     let id u pipe_src dst = map u pipe_src dst Fn.id

     let filter_map u pipe_src dst f_opt =
       let pipe_dst = add_detail u dst in
       pipe_src |> stream_of_pipe
       |> Lwt_stream.iter_s (map_opt_push f_opt pipe_dst) ;%lwt
       Lwt.return pipe_dst

     let map2 u pipe_src1 pipe_src2 dst f =
       let stream_src1 = stream_of_pipe pipe_src1 in
       let stream_src2 = stream_of_pipe pipe_src2 in
       let stream12 = product_stream_bg stream_src1 stream_src2 in
       let pipe_dst = add_detail u dst in
       stream12 |> Lwt_stream.iter_p (map2_opt_push f pipe_dst) ;%lwt
       close u dst ;
       Lwt.return pipe_dst

     let filter_map2 u pipe_src1 pipe_src2 dst f_opt =
       let stream_src1 = stream_of_pipe pipe_src1 in
       let stream_src2 = stream_of_pipe pipe_src2 in
       let stream12 = product_stream_bg stream_src1 stream_src2 in
       let pipe_dst = add_detail u dst in
       stream12 |> Lwt_stream.iter_s (map2_opt_opt_push f_opt pipe_dst) ;%lwt
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
*)

(* Payload_to_message *)

(* all the folling function is human-craft due to the lack of `pair.fmap`, `tuple3.fmap` ... *)
(* let mk_injo fmap f =
   let f' p = Some (inj (f p)) in
   fmap f' None *)
(* let mk_opt fmap f_opt =
   let f' p = Option.map (f_opt p) ~f:inj in
   fmap f' None *)
(* let fmap_injo f = mk_injo fmap f
   let fmap2_injo f = mk_injo fmap2 f *)
(* let mk_seq f_injo f_payload f_out msg =
   msg |> f_injo f_payload |> map_out f_out *)
(* let seq f push = mk_seq fmap_injo f push *)
(* let seq2 f push = mk_seq fmap2_injo f push *)
(* let seq_opt f_opt push = mk_seq fmap_opt f_opt push *)
(* let seq2_opt f_opt push = mk_seq fmap2_opt f_opt push *)

(*

   module User_level_to_key_bg
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
        and type 'a act = unit = struct
     type t = L.t
     type payload = M.payload
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

     let bind_like _u _pipe_src _dst _f = failwith "not yet"
   end
*)

(*

   module User_level_to_bg
       (L : Low_level)
       (M : M_sig with type message = L.message)
       (U : User_level
              with type t = L.t
               and type key = L.key
               and type payload = M.payload
               and type 'a act = unit Lwt.t) :
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

     let bind_like t pipe_src dst f = U.bind_like t pipe_src f dst |> bg
   end
*)

(*
   module User_level_to_key
       (L : Low_level)
       (M : M_sig with type message = L.message)
       (U : User_level
              with type t = L.t
               and type key = L.key
               and type pipe = L.detail
               and type payload = M.payload
               and type 'a act = unit Lwt.t) :
     User_level
       with type t = L.t
        and type key = L.key
        and type pipe = L.key
        and type payload = M.payload
        and type 'a act = unit Lwt.t = struct
     type t = L.t
     type key = L.key
     type pipe = L.key
     type payload = M.payload
     type 'a act = unit Lwt.t

     include User_level_common (L) (M)

     let to_pipe t key = L.find_detail t key
     let one_shot t key_dst v = U.one_shot t key_dst v
     let id t key_src key_dst = U.id t (to_pipe t key_src) key_dst
     let map t key_src key_dst f = U.map t (to_pipe t key_src) key_dst f

     let filter_map t key_src key_dst f =
       U.filter_map t (to_pipe t key_src) key_dst f

     let join t key_srcs key_dst =
       U.join t (List.map ~f:(to_pipe t) key_srcs) key_dst

     let iter t key_dst f = U.iter t (to_pipe t key_dst) f

     let map2 t key_src1 key_src2 key_dst f =
       U.map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f

     let filter_map2 t key_src1 key_src2 key_dst f =
       U.filter_map2 t (to_pipe t key_src1) (to_pipe t key_src2) key_dst f

     let bind_like t key_src key_dst f  =
       U.bind_like t (to_pipe t key_src) key_dst f 
   end
*)

(* module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
     module Fg = Make0 (Key) (M)
     include Fg

     module Bg = struct
       include Fg.L
       include User_level_to_bg (Fg.L) (M) (Fg)
     end
   end *)

(* module Make_use_key (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
     module M = Payload_as_message (P)
     module U = Make (Key) (M)
     include U.L

     (* include User_level_to_key_bg (U.L) (M) (U.Fg) *)
     module U_key_fg = User_level_to_key (U.L) (M) (U.Fg)
     include User_level_to_bg (U.L) (M) (U_key_fg)
   end *)

(* module Make_control_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
     module U = Make_control (Key) (P)
     include U.L
     module U_key = User_level_to_key (U.L) (U.M) (U)
     include User_level_to_bg (U.L) (U.M) (U_key)
   end *)

(*
  let bind_like_r u src dst f =
    let pipe_src = find_detail u src in
    let pipe_src = stream_of_detail pipe_src in
    let pipe_dst = find_detail u dst in
    let f_here p =
      match f p with
      | Some key_src2 ->
          let pipe_src2 = find_detail u key_src2 in
          Lwt_stream.iter_s
            (M.fmap (fun p -> push u dst (Some p)) () |> then_lwt)
            (stream_of_detail pipe_src2)
      | None -> Lwt.return_unit
    in

    let f_iter msg = (M.fmap f_here Lwt.return_unit) msg in
    Lwt_stream.iter_p f_iter pipe_src |> bg ;
    pipe_dst
  *)

(*
     let _bind0 (stream, push) src1 f =
     Lwt_stream.iter_p
       (fun v1 ->
         let src2 = f v1 in
         Lwt_stream.iter (fun v2 -> push (Some v2)) src2)
       src1
     >|= (fun () -> push None)
     |> bg ;
     stream
  *)

(* After `src_status` has a control message for `status_on`,
   reading message from `src_v`.
   write message from `src_v` to `dst`
*)
(*
   module Payload_to_message_not_used (P : P_sig) = struct
     type payload = P.payload
     type control = unit
     type message = Payload of payload | Control of control

     let equal_message m1 m2 =
       match (m1, m2) with
       | Payload p1, Payload p2 -> P.equal_payload p1 p2
       | Control _, Control _ -> true
       | _, _ -> false

     let _not_here : message = Control ()
     let map_out f = Option.value_map ~default:() ~f
     let inj p = Payload p
     let prj_opt = function Payload p -> Some p | Control _ -> None
     let fmap f d msg = match msg with Payload p -> f p | Control _ -> d

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

     let lift_f_payload f msg =
       match msg with Payload p -> Payload (f p) | Control c -> Control c

     let lift_f_payload_opt f msg =
       match msg with
       | Payload p -> ( match f p with Some p -> Some (Payload p) | None -> None)
       | Control c -> Some (Control c)

     let filter_payload_pair f (msg1, msg2) =
       match (msg1, msg2) with
       | Payload p1, Payload p2 -> Some (Payload (f (p1, p2)))
       | _ -> None

     let filter_payload_pair_opt f (msg1, msg2) =
       match (msg1, msg2) with
       | Payload p1, Payload p2 -> (
           match f (p1, p2) with Some p -> Some (Payload p) | None -> None)
       | _ -> None

     let filteri_payload_opt f (i, msg) =
       match msg with Payload p -> Some (Payload (f (i, p))) | _ -> None
   end
*)

(* message derived *)
(* let map_out f = Option.value_map ~default:() ~f *)
(* val seq_opt :
   (payload -> payload option) -> (message -> unit) -> message -> unit *)
(* let seq_opt f_opt push msg = msg |> fmap_opt f_opt |> map_out push *)
(* let fmap_opt f_opt = fmap (fun p -> Option.map (f_opt p) ~f:inj) None *)
(*
     val seq2_opt :
     (payload * payload -> payload option) ->
     (message -> unit) ->
     message * message ->
     unit
  *)
(* let fmap2_opt f_opt msg =
   msg |> fmap2 (fun p -> Option.map (f_opt p) ~f:inj) None *)
(* let seq2_opt f_opt f2 x = match f_opt x with Some v -> f2 v | _ -> () *)
(* let seq2_opt f_opt push msg = msg |> fmap2_opt f_opt |> map_out push  *)

(* let seqi f push (i, msg) =
   msg |> fmap (fun p -> Some (inj (f (i, p)))) None |> map_out push *)
(* let map_push f pipe = M.seq f (L.get_push pipe) |> then_lwt
    let mapi_push f pipe = M.seqi f (L.get_push pipe) |> then_lwt
    let map2_opt_push f pipe = M.seq2 f (L.get_push pipe) |> then_lwt

      val seq2 :
     (payload * payload -> payload) ->
     (message -> unit) ->
     message * message ->
     unit


   (* let seq2 f push msg =
     msg |> fmap2 (fun p -> Some (inj (f p))) None |> map_out push *)
*)
(*
     filter_map s f1 |> iter f2
     ===
     iter s (f1 . f2)
  *)

(* let with_handling_state0 pipe_dst f msg =
   if N.equal pipe_dst.status N.Running
   then (
     (match msg with
     | M.Set_state s' -> pipe_dst.status <- N.safe_transform pipe_dst.status s'
     | _ -> ()) ;
     f msg)
   else Lwt.return_unit *)

(* used state pattern
   Leaf
      U.push_state unroll target N.Fail
      U.push_state unroll target N.Done
   map, filter_map, filter_map2
      promote_result: fail-cascading
   promote_result is an error-status-carrying payload messaging
*)

(* let map u src dst f =
   let pipe_dst = find_detail u dst in
   let state_aware_f = with_handling_state0 pipe_dst (map_push f pipe_dst) in
   let on_done = iter_msg u src state_aware_f >|= fun () -> close u dst in
   on_done |> bg ;
   set_creation pipe_dst *)

(* the reason is use `push` is the consumer may come before the producer.
   The consumer needs a stream value.
   The later producer can only use the `push` to feed them into the stream.
*)

(*
    The design rationale is the stream is to provide two-folded APIs on messages and payloads.

    The streams are based on messages. If Via_message module is used, all the API for users
    are based on messages, you are free to be custumized at each API calls.

    If Via_payload is used, all the API are based are based on payload, so at the module creation
    size, you are responsible to handle non-payload (a.k.a control) messages uniformaly, and
    you can only work on payload at each API calls.

    It's less intersting to make a dummy payload that is only a message. In this case, you can
    just use Via_message.
*)

(*
     fmap f d msg ==
       msg
       |> prj_opt        // Some p | None
       |> Option.map ~f  // Some (f p) | None
       |> Option.value_map ~default:msg ~f:inj // Payload (f p) | Set_state s
  *)

(*
        (* (payload -> payload) -> (message -> 'a option) -> message -> 'a option  *)
   let seq f1 f2 msg =
     msg
     |> fmap (fun p -> Some (inj (f1 p))) None
     |> Option.value_map ~default:None ~f:f2 *)
