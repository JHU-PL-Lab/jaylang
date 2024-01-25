open Core
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

module type M_sig = sig
  type message
  type payload

  val equal_message : message -> message -> bool

  (* val equal_payload : payload -> payload -> bool *)
  val inj : payload -> message
  val prj_opt : message -> payload option

  (* val seq : 'a 'b 'c. ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
  val seq : (payload -> payload) -> (message -> unit) -> message -> unit

  val seq2 :
    (payload * payload -> payload) ->
    (message -> unit) ->
    message * message ->
    unit

  val seq_opt :
    (payload -> payload option) -> (message -> unit) -> message -> unit

  val seq2_opt :
    (payload * payload -> payload option) ->
    (message -> unit) ->
    message * message ->
    unit

  val fmap : (payload -> 'a) -> 'a -> message -> 'a
  val lift_f_payload : (payload -> payload) -> message -> message

  val lift_f_payload_opt :
    (payload -> payload option) -> message -> message option

  val filter_payload_pair :
    (payload * payload -> payload) -> message * message -> message option

  val filter_payload_pair_opt :
    (payload * payload -> payload option) -> message * message -> message option

  val filteri_payload_opt :
    ('a * payload -> payload) -> 'a * message -> message option
end

module type P_sig = sig
  type payload

  val equal_payload : payload -> payload -> bool
end

module Payload_as_message (P : P_sig) = struct
  include P

  type message = payload

  let equal_message = equal_payload
  let inj x = x
  let prj_opt x = Some x
  let seq f1 f2 x = x |> f1 |> f2
  let seq_opt f_opt f2 x = match f_opt x with Some v -> f2 v | _ -> ()
  let seq2 = seq
  let seq2_opt = seq_opt
  let fmap f _ = f
  let lift_f_payload f = f
  let lift_f_payload_opt f = f
  let filter_payload_pair f (m1, m2) = Some (f (m1, m2))
  let filter_payload_pair_opt f = f
  let filteri_payload_opt f (i, msg) = Some (f (i, msg))
end

module Payload_to_message (P : P_sig) = struct
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

module N = Naive_state_machine

(* let seqi f push (i, msg) =
   msg |> fmap (fun p -> Some (inj (f (i, p)))) None |> map_out push *)
(* let map_push f pipe = M.seq f (L.get_push pipe) |> then_lwt
   let mapi_push f pipe = M.seqi f (L.get_push pipe) |> then_lwt
   let map_opt_push f_opt pipe = M.seq_opt f_opt (L.get_push pipe) |> then_lwt
   let map2_opt_push f pipe = M.seq2 f (L.get_push pipe) |> then_lwt
   let map2_opt_opt_push f pipe = M.seq2_opt f (L.get_push pipe) |> then_lwt *)

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
