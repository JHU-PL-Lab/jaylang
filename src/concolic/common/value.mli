(**
  File: value.mli
  Purpose: values and environment for concolic interpretation

  Detailed description:
    The concolic evaluator works over the "Embedded" language,
    and for every concrete int and bool during interpretation, there
    is an associated symbolic expression.

    Thus, we take the Embedded values and say that ints and bools are
    stored as concolic values, i.e. a concrete value paired with a
    symbolic expression.
*)

module Make (K : Overlays.Typed_smt.KEY) : sig
  module Concolic_value : Utils.Equatable.P1 with type 'a t = 'a * ('a, K.t) Overlays.Typed_smt.t
  (** [Concolic_value] has a type ['a t] to represent concolic values: values that
      are a pair of a concrete value ['a] and symbolic expression keyed by the provided key. *)

  include module type of Lang.Value.Embedded (Concolic_value)

  include module type of T with type t = t

  val equal : t -> t -> bool Concolic_value.t
  (** [equal a b] is intensional equality of [a] and [b], comparing expressions
      in the case of ints and bools. *)
end

module Default : module type of Make (Interp_common.Step)

include module type of Default
