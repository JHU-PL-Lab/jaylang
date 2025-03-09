(**
  File: input_feeder.mli
  Purpose: use stepkeys to get concrete inputs for interpretation

  Detailed description:
    An input feeder uses a stepkey to provide back a concrete
    value to fill in the input clause during interpretation.

    Input feeders can be constant ([zero]), random ([default]), or
    from a Z3 model ([from_model model]) to satisify the constraints
    within the model.
    
  Dependencies:
    Z3_api -- the source of the model
    Stepkey -- how to identify which input is desired
*)

type t = { get : 'a. 'a Stepkey.t -> 'a } [@@unboxed]
(** [t] gets an input to go for the clause with the provided key. *)

val zero : t
(** [zero] is 0 or false. *)

val default : t
(** [default] is random in -10, 10 inclusive or a random bool. *)

module Make (Z : Z3_api.S) : sig
  val from_model : Z.model -> t
  (** [from_model model] is a feeder that satisfies the given [model]. *)
end
