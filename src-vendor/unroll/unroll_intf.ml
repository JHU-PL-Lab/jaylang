open Core

(* The library has an invariant that the stream with a key can only be create once
    What if a duplicate key is created? As a general case, it may be configurable, but in this library,
    maybe we can just raise an exception.

    This can be made by in any stream creation function. The source stream can be provided by a `Pipe.t`,
    but the destination

   Discuss 1: Explicit Lwt.t
   The reason that the type for pipe is weird because the `Lwt_stream.t` has functions that returns `'a Lwt.t`.
   These functions will lift any computations into `Lwt.t` so that the compositional type should aware both of
   them, and have forms like `'a Lwt_stream.t Lwt.t`.

   Then we can have
   let return (x : 'a) = Lwt.return (Lwt_stream.return x)
   let bind (x : 'a Lwt_stream.t Lwt.t) (f : 'a -> 'b Lwt_stream.t Lwt.t) : 'b Lwt_stream.t Lwt.t  =
     Lwt.bind x (fun s ->
       let (bss : ('b Lwt_stream.t Lwt.t) Lwt_stream.t ) = Lwt_stream.map s f in
       ...
     )

   Here we need a function converting from `('b Lwt_stream.t Lwt.t) Lwt_stream.t` to `'b Lwt_stream.t Lwt.t`

   Discuss 2: Implicit Lwt.t

   If we allow the mix with `Lwt.t` type and non `Lwt.t` type, we will have
   let return (x : 'a ) = Lwt_stream.return x
   let bind (x : 'a Lwt_stream.t) (f : 'a -> 'b Lwt_stream.t) : 'b Lwt_stream.t =
     Lwt.bind x (fun s ->
       let (bss : ('b Lwt_stream.t) Lwt_stream.t ) = Lwt_stream.map s f in
       ...
     )

   Here we need a function converting from `'b Lwt_stream.t Lwt_stream.t` to `'b Lwt_stream.t`.
   It's more straightforward but however
*)

(* Ingredient *)

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
end

module type P_sig = sig
  type payload

  val equal_payload : payload -> payload -> bool
end

module type Low_level = sig
  type t
  type message
  type key
  type detail

  val create : ?is_dedup:bool -> unit -> t
  val reset : t -> unit
  val create_key : t -> ?task:(unit -> unit) -> key -> unit
  val get_stream : t -> key -> message Lwt_stream.t
  val set_pre_push : t -> key -> (message -> message option) -> unit
  val push_msg_by_key : t -> key -> message -> unit
  val push_msg : t -> detail -> message -> unit
  val close : t -> key -> unit
  val messages_sent : t -> key -> message list
  val msg_queue : unit Lwt.t list ref
  val add_detail : t -> key -> detail
  val find_detail : t -> key -> detail
  val stream_of_detail : detail -> message Lwt_stream.t
end

module type User_level = sig
  (* shared, to substitute *)
  type t

  (* target *)
  type key

  (* abstract source *)
  type pipe
  type payload

  (* custom, as output *)
  type 'a act

  val get_payload_stream : t -> key -> payload Lwt_stream.t
  val push : t -> key -> payload option -> unit

  (* create; only immediate pipe can be created *)
  val one_shot : t -> key -> payload list -> pipe act
  val id : t -> pipe -> key -> pipe act
  val map : t -> pipe -> key -> (payload -> payload) -> pipe act
  val filter_map : t -> pipe -> key -> (payload -> payload option) -> pipe act
  (* val filter : u -> t -> key -> (payload -> bool) -> t act *)

  val map2 :
    t -> pipe -> pipe -> key -> (payload * payload -> payload) -> pipe act

  val filter_map2 :
    t ->
    pipe ->
    pipe ->
    key ->
    (payload * payload -> payload option) ->
    pipe act

  val join : t -> pipe list -> key -> pipe act
  (* iter doesn't create a new strean *)

  val iter : t -> pipe -> (payload -> unit) -> unit act
  (* val bind : u -> t -> (payload -> t) -> t act
     val bind0 : u -> t -> (payload -> key) -> t Lwt_stream.t *)
end

module type U = sig
  include Low_level
  include User_level with type t := t and type key := key
end

module type U_bg = U with type 'a act = unit

module type U_payload = sig
  include U
  module Bg : U_bg with type key = key and type payload = payload
end

module type Top_sigs = sig
  module Make_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U_payload
      with type key = Key.t
       and type payload = P.payload
       and type message = P.payload
       and type 'a act = 'a Lwt.t

  module Make_use_key (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U_bg
      with type key = Key.t
       and type message = P.payload
       and type payload = P.payload
       and type pipe = Key.t

  module Make_dummy_control (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U_payload
      with type key = Key.t
       and type payload = P.payload
       and type 'a act = 'a Lwt.t
end
