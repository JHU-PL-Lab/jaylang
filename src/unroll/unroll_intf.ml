open Core

module type M_sig = sig
  type message
  type result
  type key

  val equal_message : message -> message -> bool
end

module type S = sig
  type t
  type message
  type result
  type key

  val create : unit -> t
  val alloc_task : t -> ?task:(unit -> unit) -> key -> unit
  val by_return : t -> key -> message -> unit
  val by_iter : t -> key -> (message -> unit Lwt.t) -> unit Lwt.t
  val by_iter_u : t -> key -> (message -> unit Lwt.t) -> unit
  val by_id : t -> key -> key -> unit Lwt.t
  val by_id_u : t -> key -> key -> unit
  val by_map : t -> key -> key -> (message -> message) -> unit Lwt.t
  val by_map_u : t -> key -> key -> (message -> message) -> unit

  val by_filter_map :
    t -> key -> key -> (message -> message option) -> unit Lwt.t

  val by_filter_map_u : t -> key -> key -> (message -> message option) -> unit

  val by_map2 :
    t -> key -> key -> key -> (message * message -> message) -> unit Lwt.t

  val by_map2_u :
    t -> key -> key -> key -> (message * message -> message) -> unit

  val by_filter_map2 :
    t ->
    key ->
    key ->
    key ->
    (message * message -> message option) ->
    unit Lwt.t

  val by_filter_map2_u :
    t -> key -> key -> key -> (message * message -> message option) -> unit

  val by_join : t -> ?f:(message -> message) -> key -> key list -> unit Lwt.t
  val by_join_u : t -> key -> key list -> unit
  val by_bind : t -> key -> key -> (key -> message -> unit Lwt.t) -> unit Lwt.t
  val by_bind_u : t -> key -> key -> (key -> message -> unit Lwt.t) -> unit
  val get_messages : t -> key -> message list
end
