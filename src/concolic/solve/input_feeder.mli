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
*)

include module type of Interp_common.Input_feeder.Make (Interp_common.Step)

val of_model : Interp_common.Step.t Overlays.Typed_smt.model -> t

module Make (Z : Z3_api.S) : sig
  val from_model_and_subs : Z.model -> Expression.Subst.t list -> t
  (** [from_model_and_subs model subs] is a feeder that satisfies the given [model], and
      any key that exists in the given substitutions [subs] returns the value given in
      the substitution. *)
end
