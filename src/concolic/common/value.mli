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

  Dependencies:
    Expression -- ints and bools are stored with their expression
*)

module Concolic_value : Lang.Value.V with type 'a t = 'a * 'a Expression.t
(** [Concolic_value] has a type ['a t] to represent concolic values: values that
    are a pair of a concrete value ['a] and symbolic expression ['a Expression.t]. *)

include module type of Lang.Value.Embedded (Concolic_value)

include module type of T with type t = t

val equal : t -> t -> bool Concolic_value.t
(** [equal a b] is intensional equality of [a] and [b], comparing expressions
    in the case of ints and bools. *)
