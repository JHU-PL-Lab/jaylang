open Core
open Messages

(*
   OB1: a stream is obviously compositional. Is a named stream compositional? It should be,
   however,

   OB2: named operation for `t` includes `get: name -> t` and `set: name -> t -> ()`.
   We can fuse `get` into the exisiting `t` but we may not fuse `set` into it.

   OB3: a compositional thing should have a compositional type.

   OB4:
   Now the remaining question is when we get a stream and apply a function, shall we apply 
   an `f_payload` or an `f_message`, especially the `f` is done in multiple steps e.g.

   let s3 = bind_like s1 (fun v -> s2) in
   set s3 k3

   OB5:
   there is some invariant e.g.

   set s k := iter s (push k)
   map s f1 |> iter f2 == iter s (f1 . f2)

   which may hint we just need 

   OB6:
   user specifies a function on payload `f_payload`
   specific pipes are in charge of control messages

   pipe receives control messages from its source and only send message on its own status, which means
   the above iter (in `set`) is a special set.

   e.g.
   s1 --map-f-> s2 
                  \
                   s5
                  /
   s3 --map-f-> s4
*)

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

(* Any of these User_level functions has a motivation, to create a stream with a key
   e.g. unlike the normal
   fmap : 'a t -> ('a -> 'b) -> 'b t
     instead we have
   map : pipe -> key -> ('a -> 'b) -> pipe
     where `pipe` can be a fixed `_ t` but the key will points to the returning `pipe`
   since key can point to that, it's also ok to return `unit` therefore it can look like
   map : pipe -> key -> ('a -> 'b) -> ()

   Applying the same idea on `bind` where the normal bind is
   bind : 'a t -> ('a -> 'b t) -> 'b t
   we should have
   bind : pipe -> ('a -> pipe) -> pipe

   the key binding is the second step to deal with the pipe, the essential idea here is to
   lisiten to the first pipe, deriving other new pipes and other join the results of the other
   pipes
*)

(* val bind0_pipe :
   t ->
   ?sp:'b Lwt_stream.t * ('b option -> unit) ->
   'a Lwt_stream.t ->
   ('a -> 'b Lwt_stream.t option) ->
   'b Lwt_stream.t *)

module type Low_level = sig
  type t
  type message
  type key
  type detail

  val create : ?is_dedup:bool -> unit -> t
  val reset : t -> unit
  val create_key : t -> ?task:(unit -> unit) -> key -> unit
  val get_stream : t -> key -> message Lwt_stream.t
  val get_status : t -> key -> N.t
  val set_pre_push : t -> key -> (key -> message -> message option) -> unit
  val push_msg : t -> key -> message -> unit
  val get_push : key -> detail -> message -> unit
  val close : t -> key -> unit
  val messages_sent : t -> key -> message list
  val msg_queue : unit Lwt.t list ref
  val dump : t -> unit
  val stream_of_key : t -> key -> message Lwt_stream.t

  (* detail related *)
  val add_detail : t -> key -> detail
  val find_detail : t -> key -> detail
  val set_creation : detail -> unit
  val stream_of_detail : detail -> message Lwt_stream.t
  (* val push_msg : t -> detail -> message -> unit *)
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
  val get_payloads : t -> key -> payload list Lwt.t
  val get_available_payloads : t -> key -> payload list

  val set_pre_push_payload :
    t -> key -> (key -> payload -> payload option) -> unit

  val push : t -> key -> payload option -> unit
  val set_status : t -> key -> N.t -> unit
  (* iter doesn't create a new strean *)

  val iter : t -> pipe -> (payload -> unit) -> unit act
  (* val bind : u -> t -> (payload -> t) -> t act
     val bind0 : u -> t -> (payload -> key) -> t Lwt_stream.t *)
end

module type U = sig
  include Low_level

  include
    User_level
      with type t := t
       and type key := key
       and type pipe := key
       and type 'a act = unit

  (* TODO: use array syntax? *)
  val set : t -> key -> message Lwt_stream.t -> unit
  val map : t -> key -> (payload -> payload) -> message Lwt_stream.t
  val id : t -> key -> message Lwt_stream.t

  val filter_map :
    t -> key -> (payload -> payload option) -> message Lwt_stream.t
  (* val filter : u -> t -> key -> (payload -> bool) -> t act *)

  val map2 :
    t -> key -> key -> (payload * payload -> payload) -> message Lwt_stream.t

  val filter_map2 :
    t ->
    key ->
    key ->
    (payload * payload -> payload option) ->
    message Lwt_stream.t

  val join : t -> key list -> message Lwt_stream.t

  val joini :
    t -> key list -> (int * payload -> payload) -> message Lwt_stream.t

  val one_shot : t -> payload list -> message Lwt_stream.t

  (* val bind : t -> pipe -> key -> (payload -> pipe) -> pipe act *)
  val bind_like : t -> key -> (payload -> key option) -> message Lwt_stream.t
  val on : t -> key -> N.t -> key -> message Lwt_stream.t
  val bind_like_list : t -> key -> (payload -> key list) -> message Lwt_stream.t
end

module type Top_sigs = sig
  module Make_just_payload (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U
      with type key = Key.t
       and type message = P.payload
       and type payload = P.payload

  module Make_dummy_control (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U with type key = Key.t and type payload = P.payload

  module Make_stateful (Key : Base.Hashtbl.Key.S) (P : P_sig) :
    U with type key = Key.t and type payload = P.payload
end
