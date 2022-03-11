open! Core
open Types

include module type of struct
  include State
end

include module type of struct
  include Info
end

val create_state : Tracelet.t -> Id.t -> State.t

val init_node :
  State.t ->
  Lookup_key.t ->
  Node.ref_t ->
  Node.ref_t * Lookup_result.t Lwt_stream.t

val clear_phis : State.t -> unit

(* val refresh_picked : State.t -> Z3.Model.model -> unit *)
val add_phi : State.t -> Lookup_key.t -> Z3.Expr.expr -> unit
val find_or_add_node : State.t -> Lookup_key.t -> Tracelet.t -> Node.ref_t
val find_node_exn : State.t -> Lookup_key.t -> Tracelet.t -> Node.ref_t

val find_or_add :
  State.t ->
  Lookup_key.t ->
  Tracelet.t ->
  Node.ref_t ->
  bool * Node.ref_t * Lookup_result.t Lwt_stream.t

val pvar_picked : State.t -> Lookup_key.t -> bool
val get_lookup_stream : State.t -> Lookup_key.t -> Lookup_result.t Lwt_stream.t

val get_lookup_pusher :
  State.t -> Lookup_key.t -> Lookup_result.t option -> unit
