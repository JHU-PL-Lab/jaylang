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
module type M_sig = sig
  type message

  val equal_message : message -> message -> bool
end

module type P_sig = sig
  type payload

  val equal_payload : payload -> payload -> bool
end

module type Lifter = sig
  type 'b t_in
  type 'c t_out

  val lift1 : ('a1 -> 'b t_in) -> 'a1 -> 'c t_out
  val lift2 : ('a1 -> 'a2 -> 'b t_in) -> 'a1 -> 'a2 -> 'c t_out
  val lift3 : ('a1 -> 'a2 -> 'a3 -> 'b t_in) -> 'a1 -> 'a2 -> 'a3 -> 'c t_out

  val lift4 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b t_in) ->
    'a1 ->
    'a2 ->
    'a3 ->
    'a4 ->
    'c t_out

  val lift5 :
    ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b t_in) ->
    'a1 ->
    'a2 ->
    'a3 ->
    'a4 ->
    'a5 ->
    'c t_out
end

(* Public *)
module type Common = sig
  type t
  type message
  type key
  type detail

  val create : unit -> t
  val reset : t -> unit
  val create_key : t -> ?task:(unit -> unit) -> key -> unit
  val get_stream : t -> key -> message Lwt_stream.t
  val set_pre_push : t -> key -> (message -> message option) -> unit
  val just_push : t -> key -> message option -> unit
  val real_push : t -> key -> message -> unit
  val real_close : t -> key -> unit
  val push_all : t -> key -> message list -> unit
  val messages_sent : t -> key -> message list
  val msg_queue : unit Lwt.t list ref
  val add_detail : t -> key -> detail
  (* val seq : (_ -> _) -> (_ -> _) -> _ *)
end

module type Use = sig
  (* shared, to substitute *)
  type t
  type key

  (* whether
     1. use a `message` that shares the same name with common, to substitute
     2. use a unique name *)
  type payload

  (* custom, as output *)
  type 'a act

  (* internal *)
  type pipe

  (* create; only immediate pipe can be created *)
  val one_shot : t -> key -> payload -> pipe act
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

  val iter : t -> pipe -> (payload -> unit act) -> unit act
  (* val bind : u -> t -> (payload -> t) -> t act
     val bind0 : u -> t -> (payload -> key) -> t Lwt_stream.t *)
end

(* S is a derived interface, rather than a general interface *)
module type S = sig
  include Common
  include Use with type t := t and type key := key
end

module type S_bg = S with type 'a act = unit
