
type t = Concolic_key.t -> int
(** [t] gets an input to go for the clause with the provided key. *)

val zero : t
(** [zero _] is 0. *)

val default : t
(** [default _] is random in -10, 10 inclusive. *)

val from_model : Z3.Model.model -> t
(** [from_model model] is a feeder that satisfies the given [model]. *)
