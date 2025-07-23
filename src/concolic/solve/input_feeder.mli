(**
  File: input_feeder.mli
  Purpose: use stepkeys to get concrete inputs for interpretation

  Detailed description:
    An input feeder uses a stepkey to provide back a concrete
    value to fill in the input clause during interpretation.

    Input feeders can be constant ([zero]), random ([default]), or
    from an smt model ([from_model model]) to satisify the constraints
    within the model.
*)

include module type of Interp_common.Input_feeder.Make (Interp_common.Step)

val of_model : Interp_common.Step.t Overlays.Typed_smt.model -> t
