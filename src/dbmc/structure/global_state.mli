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
