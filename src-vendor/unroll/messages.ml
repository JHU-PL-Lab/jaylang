open Core

module type P_sig = sig
  type payload

  val equal_payload : payload -> payload -> bool
end

module type M_sig = sig
  type message
  type payload

  val equal_message : message -> message -> bool

  (* val equal_payload : payload -> payload -> bool *)
  val inj : payload -> message
  val prj_opt : message -> payload option
  val fmap : (payload -> 'a) -> 'a -> message -> 'a
  val fmap2 : (payload * payload -> 'a) -> 'a -> message * message -> 'a
  val map_payload : (payload -> payload) -> message -> message

  val filter_map_payload :
    (payload -> payload option) -> message -> message option
end

module Payload_as_message (P : P_sig) = struct
  include P

  type message = payload

  let equal_message = equal_payload
  let inj x = x
  let prj_opt x = Some x
  let fmap f _ = f
  let fmap2 f _ = f
  let map_payload f = f
  let filter_map_payload f = f
end

module N = Naive_state_machine

module Payload_to_message (P : P_sig) = struct
  type payload = P.payload
  type message = Payload of payload | Set_state of N.t

  let equal_message m1 m2 =
    match (m1, m2) with
    | Payload p1, Payload p2 -> P.equal_payload p1 p2
    | Set_state _, Set_state _ -> true
    | _, _ -> false

  let inj p = Payload p
  let prj_opt = function Payload p -> Some p | Set_state _ -> None
  let fmap f d msg = match msg with Payload p -> f p | Set_state _ -> d

  let map_payload f msg =
    match msg with Payload p -> Payload (f p) | Set_state s -> Set_state s

  (* TODO: should be derived by Pair.lift_map *)
  let fmap2 f d (msg1, msg2) =
    match (msg1, msg2) with Payload p1, Payload p2 -> f (p1, p2) | _, _ -> d

  (* but keep state *)
  let filter_map_payload f_opt msg =
    match msg with
    | Payload p -> (
        match f_opt p with Some p -> Some (Payload p) | None -> None)
    | Set_state s -> Some (Set_state s)
end
