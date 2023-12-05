open Core

module type M_sig = sig
  type message
  type payload

  val equal_message : message -> message -> bool
end

module type P_sig = sig
  type payload

  val equal_payload : payload -> payload -> bool
end

module type Part_C = sig
  type t
  type message
  type payload
  type key

  val create : unit -> t
  val reset : t -> unit
  val create_key : t -> ?task:(unit -> unit) -> key -> unit
  val get_stream : t -> key -> message Lwt_stream.t
  val set_pre_push : t -> key -> (message -> message option) -> unit
  val just_push : t -> key -> message option -> unit
  val push_all : t -> key -> message list -> unit
  val messages_sent : t -> key -> message list
  val msg_queue : unit Lwt.t list ref
end

module type Part_T = sig
  type t
  type message
  type key
  type act

  val one_shot : t -> key -> message -> act
  val by_iter : t -> key -> (message -> unit Lwt.t) -> act
  val by_id : t -> key -> key -> act
  val by_map : t -> key -> key -> (message -> message) -> act
  val by_filter_map : t -> key -> key -> (message -> message option) -> act
  val by_map2 : t -> key -> key -> key -> (message * message -> message) -> act

  val by_filter_map2 :
    t -> key -> key -> key -> (message * message -> message option) -> act

  val by_join : t -> ?f:(message -> message) -> key -> key list -> act
  val by_bind : t -> key -> key -> (key -> message -> act) -> act
end

module type All = sig
  include Part_C

  include
    Part_T with type t := t and type message := message and type key := key
end

module type S = sig
  include All with type act := unit Lwt.t

  module No_wait :
    All
      with type t := t
       and type message := message
       and type key := key
       and type act := unit
end

module type S2 = sig
  include Part_C

  include
    Part_T with type t := t and type message := payload and type key := key
end
