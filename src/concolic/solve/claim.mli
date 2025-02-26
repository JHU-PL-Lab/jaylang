(**
  File: claim.mli
  Purpose: constrain program branches with symbolic expressions

  Detailed description:
    A "claim" is an equality of some symbolic expression with a
    direction. That is, some direction taken down a program branch
    is only feasible if the associated expression is satisfiable.

    This disregards any path constrains *above* the branch. It is
    strictly a claim about that program point.

  Dependencies:
    Expression
    Direction
*)

type 'a t = Equality of ('a Expression.t * 'a Direction.t) [@@unboxed]
(** ['a t] is an equality of a symbolic expression with a branch direction. *)

val flip : bool t -> bool t
(** [flip claim] flips the direction of the [claim], and the expression is unaffected. *)

val direction : 'a t -> 'a Direction.t
(** [direction claim] is the direction inside the [claim]. *)

val to_expression : 'a t -> bool Expression.t
(** [to_expression claim] is an expression that actually equates the expression inside
    the [claim] with the direction. *)
