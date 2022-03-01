open! Core
open Types

include module type of struct
  include State
end

include module type of struct
  include Info
end

val create_state : Tracelet.t -> Id.t -> State.t
val clear_phis : State.t -> unit

(* val refresh_picked : State.t -> Z3.Model.model -> unit *)
val add_phi : State.t -> Lookup_key.t -> Z3.Expr.expr -> unit

val find_or_add :
  State.t -> Lookup_key.t -> Tracelet.t -> Node.ref_t -> bool * Node.ref_t

val pvar_picked : State.t -> Lookup_key.t -> bool
