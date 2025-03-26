(**
  File: value.mli
  Purpose: values and environment for concolic interpretation

  Detailed description:
    The concolic evaluator works over the "Embedded" language,
    and for every concrete int and bool during interpretation, there
    is an associated symbolic expression.

    Thus, we take the Embedded values and say that ints and bools are
    stored as [int * int Expression.t] and [bool * bool Expression.t],
    respectively.

  Dependencies:
    Expression -- ints and bools are stored with their expression
*)

include module type of Lang.Value.Embedded (struct type 'a t = 'a * 'a Expression.t let to_string f (v, _) = f v end)

include module type of T with type t = t
