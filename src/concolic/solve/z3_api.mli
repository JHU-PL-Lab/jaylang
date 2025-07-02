(**
  File: z3_api.mli
  Purpose: an interface to Z3 for the concolic evaluator

  Detailed description:
    The concolic evaluator uses the Z3 SMT solver to solve constraints.
    It needs only integer and boolean symbolic expressions, as well
    as some binary operations. That is what is supported here.

    Expressions can be built, identified with stepkeys, and solved for
    through this interface.

    Much of this behavior comes from Utils.Typed_z3, which includes
    everything that is independent of concolic evaluation (e.g.
    it includes all except the stepkeys).

    The behavior here is used by [Solve] and [Expression] in a
    sort of layered architecture. Any solving done by the concolic
    evaluator is through those modules.
*)

module type S = sig
  type model

  include Overlays.Typed_z3.S with type model := model

  val var_of_key : 'a Interp_common.Key.Stepkey.t -> 'a t
  val value_of_key : model -> 'a Interp_common.Key.Stepkey.t -> 'a option
end

module Make () : S