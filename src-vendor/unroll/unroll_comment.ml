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

(* let join_both t key_dst key_src_pairs f : unit Lwt.t =
      let srcs =
        List.map key_src_pairs ~f:(fun (a, b) ->
            let s1, s2 = (get_stream t a, get_stream t b) in
            product_stream s1 s2)
      in
      let cb = push_msg t key_dst in
      Lwt_list.iter_s
        (Lwt_stream.iter_s (fun v ->
             cb (f v) ;
             Lwt.return_unit))
        srcs *)

(* module type T = sig
     type t
     type q

     val eq_t : t -> t -> bool
     val eq_q : q -> q -> bool
   end

   module type R = sig
     type r

     include T with type t := r
   end

   module type W = sig
     type w

     val w0 : w
   end

   module My_T = struct
     type t = Taa

     let eq_t _t1 _t2 : bool = true
     let eq_q _t1 _t2 : bool = true
   end

   module W_Make (W : W) = struct
     include My_T

     let _ = eq_q W.w0 W.w0
   end *)

(* module Make_pipe0 (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
     module Message = Payload_to_message (P)
     module Common = Low_level (Key) (Message)
   end *)

(*
   module type Lifter = sig
     type 'a key_p
     type 'a key_q
     type 'a ret_p
     type 'a ret_q

     val lift0 : 'a ret_p -> 'a ret_q
     val lift1 : ('a1 key_p -> 'a ret_p) -> 'a1 key_q -> 'a ret_q

     val lift2 :
       ('a1 key_p -> 'a2 key_p -> 'a ret_p) -> 'a1 key_q -> 'a2 key_q -> 'a ret_q

     val lift3 :
       ('a1 key_p -> 'a2 key_p -> 'a3 key_p -> 'a ret_p) ->
       'a1 key_q ->
       'a2 key_q ->
       'a3 key_q ->
       'a ret_q

     val lift4 :
       ('a1 key_p -> 'a2 key_p -> 'a3 key_p -> 'a4 key_p -> 'a ret_p) ->
       'a1 key_q ->
       'a2 key_q ->
       'a3 key_q ->
       'a4 key_q ->
       'a ret_q

     val lift5 :
       ('a1 key_p -> 'a2 key_p -> 'a3 key_p -> 'a4 key_p -> 'a5 key_p -> 'a ret_p) ->
       'a1 key_q ->
       'a2 key_q ->
       'a3 key_q ->
       'a4 key_q ->
       'a5 key_q ->
       'a ret_q
   end *)

(* .ml *)

(* (L : Lifter
            with type 'a ret_p = unit U.act
             and type 'a ret_q = unit
             and type 'a key_p = 'a
             and type 'a key_q = 'a) *)
(* module L_lwt_unit_to_unit = struct
     type 'a key_p = 'a
     type 'a key_q = 'a
     type 'a ret_p = unit Lwt.t
     type 'a ret_q = unit

     let lift0 f = Lwt.async (fun () -> f)
     let lift1 f a1 = Lwt.async (fun () -> f a1)
     let lift2 f a1 a2 = Lwt.async (fun () -> f a1 a2)
     let lift3 f a1 a2 a3 = Lwt.async (fun () -> f a1 a2 a3)
     let lift4 f a1 a2 a3 a4 = Lwt.async (fun () -> f a1 a2 a3 a4)
     let lift5 f a1 a2 a3 a4 a5 = Lwt.async (fun () -> f a1 a2 a3 a4 a5)
   end *)

(* module L_lwt_to_unit = struct
     type 'a key_p = 'a
     type 'a key_q = 'a
     type 'a ret_p = 'a Lwt.t
     type 'a ret_q = unit

     open Lwt.Infix

     let lift0 f = Lwt.async (fun () -> f >>= fun _ -> Lwt.return_unit)
     let lift1 f a1 = Lwt.async (fun () -> f a1 >>= fun _ -> Lwt.return_unit)
     let lift2 f a1 a2 = Lwt.async (fun () -> f a1 a2 >>= fun _ -> Lwt.return_unit)

     let lift3 f a1 a2 a3 =
       Lwt.async (fun () -> f a1 a2 a3 >>= fun _ -> Lwt.return_unit)

     let lift4 f a1 a2 a3 a4 =
       Lwt.async (fun () -> f a1 a2 a3 a4 >>= fun _ -> Lwt.return_unit)

     let lift5 f a1 a2 a3 a4 a5 =
       Lwt.async (fun () -> f a1 a2 a3 a4 a5 >>= fun _ -> Lwt.return_unit)
   end *)

(* U.iter is the old iter which iterates `f` on `key_src`
   This function is to make a new iter which uses U.iter but with the modified return type.

   `f` is the worker function, `payload -> unit Lwt.t`
   `U.iter` will finally return `unit Lwt.t`.

   `iter` should return `unit` finally.
   Luckily with `L_lwt_to_unit.lift3`, it should be able to _lift_ `unit Lwt.t` to `unit`.
*)

(* module Make_payload_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
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
   end *)

(*
   module Make_pipe_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
     module MP = Make_payload (Key) (P)
     include MP
     include Change_use_lwt_to_unit (MP) 
   end *)

(* module Make_payload_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) :
   U_bg
     with type key = Key.t
      and type message = P.payload
      and type payload = P.payload
      and type pipe = Key.t *)

(*
   module Make_pipe_bg (Key : Base.Hashtbl.Key.S) (P : P_sig) :
     U_bg with type key = Key.t and type payload = P.payload *)

(* let from_filter f msg = if f msg then Some msg else None
   let to_filter f msg = Option.is_some (f msg)
   let join_map t key_dst key_srcs f = join t ~f key_dst key_srcs
   let join_map_u t key_dst key_srcs f =
   Lwt.async (fun () -> join t ~f key_dst key_srcs) *)

(* old module Pipe *)
(* module Pipe = struct
      open Message

      type pipe = detail

      let stream_of_pipe pipe = Lwt_stream.clone pipe.stream

      let get_payload_stream u p =
        get_stream u p |> Lwt_stream.filter_map Message.prj_opt

      (* let bg _t x = Lwt.async (fun () -> x >>= fun _ -> Lwt.return_unit) *)

      let push t key p_opt =
        match p_opt with
        | Some p -> push_msg t key (Message.inj p)
        | None -> close t key

      let one_shot u dst vs =
        let pipe_dst = add_detail u dst in
        List.iter vs ~f:(fun v -> pipe_dst.push (Some (inj v))) ;
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
        stream12 |> Lwt_stream.iter_s (seq2 f (get_push pipe_dst) |> then_lwt) ;%lwt
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
    end *)

(* (* module Message = Payload_as_message (P)
   module Common = Low_level (Key) (Message)

   module All = struct
     include Common

     type 'a act = 'a Lwt.t
     type payload = P.payload

     module Pipe = struct
       open Message

       type pipe = detail

       let stream_of_pipe pipe = Lwt_stream.clone pipe.stream
       let get_payload_stream u p = get_stream u p

       (* let bg _t x = Lwt.async (fun () -> x >>= fun _ -> Lwt.return_unit) *)

       let push t key p_opt =
         match p_opt with Some p -> push_msg t key p | None -> close t key

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
         stream12 |> Lwt_stream.iter_s (seq2 f (get_push pipe_dst) |> then_lwt) ;%lwt
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

     include Pipe
   end *) *)

(*
module type S_legacy_just_payload = sig
  include U

  module Bg :
    U_bg
      with type key = key
       and type message = message
       and type payload = message
       and type pipe = key
end

       module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) :
   S_legacy_just_payload
     with type key = Key.t
      and type message = P.payload
      and type payload = P.payload
      and type pipe = Key.t
      and type 'a act = unit Lwt.t
*)
(*
   module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) = struct
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

     let push t key p_opt =
       match p_opt with Some p -> push_msg t key (Fn.id p) | None -> close t key

     module Bg = struct
       include M0.Common

       include Change_use_lwt_unit_to_unit (struct
         type nonrec t = t
         type nonrec key = key

         include M0.Use
       end)
     end
   end *)

(* module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) :
   S
     with type key = Key.t
      and type message = M.message
      and type 'a act = unit Lwt.t *)

(*
         (* now it looks more meaningful to me where the `unit Lwt.t` means the things of a stream (is done) *)
   module Change_use_lwt_unit_to_unit (U : Use with type 'a act = unit Lwt.t) =
   struct
     type payload = U.payload
     type pipe = U.pipe
     type 'a act = unit

     let bg x = Lwt.async (fun () -> x)
     let get_payload_stream = U.get_payload_stream
     let push = U.push
     let one_shot t key_src v = U.one_shot t key_src v |> bg
     let id t key_src key_dst = U.id t key_src key_dst |> bg
     let iter t key_src f = U.iter t key_src f |> bg
     let map t key_src key_dst f = U.map t key_src key_dst f |> bg
     let filter_map t key_src key_dst f = U.filter_map t key_src key_dst f |> bg
     let join t key_srcs key_dst = U.join t key_srcs key_dst |> bg

     let map2 t key_src1 key_src2 key_dst f =
       U.map2 t key_src1 key_src2 key_dst f |> bg

     let filter_map2 t key_src1 key_src2 key_dst f =
       U.filter_map2 t key_src1 key_src2 key_dst f |> bg
   end
*)

(*

   module Part_use_unit (C : Common) (Key : Base.Hashtbl.Key.S) (M : M_sig) =
   struct
     (* module C = Low_level (Key) (M) *)
     open C

     type payload = M.message
     type pipe = Key.t
     type 'a act = unit Lwt.t

     let seq f push x = x |> f |> push
     let map_in f push x = match f x with Some v -> push v | None -> ()
     let get_payload_stream = get_stream

     let push t key p_opt =
       match p_opt with Some p -> push_msg t key (Fn.id p) | None -> close t key

     let one_shot t key vs =
       List.iter vs ~f:(push_msg t key) ;
       close t key ;
       Lwt.return_unit

     let id t key_src key_dst : unit Lwt.t =
       let stream_src = get_stream t key_src in
       Lwt_stream.iter_s (push_msg t key_dst |> then_lwt) stream_src

     let map t key_src key_dst f : unit Lwt.t =
       let stream_src = get_stream t key_src in
       Lwt_stream.iter_s (seq f (push_msg t key_dst) |> then_lwt) stream_src

     let filter_map t key_src key_dst f : unit Lwt.t =
       let stream_src = get_stream t key_src in
       Lwt_stream.iter_s (map_in f (push_msg t key_dst) |> then_lwt) stream_src

     let join t key_srcs key_dst =
       let stream_srcs = List.map key_srcs ~f:(get_stream t) in
       Lwt_stream.choose stream_srcs |> Lwt_stream.iter (push_msg t key_dst)

     let map2 t key_src1 key_src2 key_dst f : unit Lwt.t =
       let stream_src1 = get_stream t key_src1 in
       let stream_src2 = get_stream t key_src2 in

       (* seq2 *)
       Lwt_stream.iter_p
         (seq f (push_msg t key_dst) |> then_lwt)
         (product_stream_bg stream_src1 stream_src2)
       >|= fun () -> close t key_dst

     let filter_map2 t key_src1 key_src2 key_dst f : unit Lwt.t =
       let stream_src1 = get_stream t key_src1 in
       let stream_src2 = get_stream t key_src2 in

       (* seq2_opt *)
       Lwt_stream.iter
         (map_in f (push_msg t key_dst))
         (product_stream_bg stream_src1 stream_src2)

     let iter t key_src f =
       let stream_src = get_stream t key_src in
       Lwt_stream.iter_s (f |> then_lwt) stream_src
   end

   module Make0 (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
     module Common = Low_level (Key) (M)
     module Use = Part_use_unit (Common) (Key) (M)
   end

   module Make (Key : Base.Hashtbl.Key.S) (M : M_sig) = struct
     module M0 = Make0 (Key) (M)
     include M0.Common
     include M0.Use
   end
*)
