(**
  File: direction.mli
  Purpose: represent the direction of a program branch

  Detailed description:
    The Embedded language has n-ary integer cases as well
    as traditional boolean if/else conditions. Since program
    paths are a list of directions, and the concolic evaluator
    solves for program paths, we need to represent these
    two kinds of directions.

    We get reuse by using a GADT, so we can have [int] directions
    and [bool] directions.
*)

module T : sig
  type _ t =
    | True_direction : bool t
    | False_direction : bool t
    | Case_int : int -> int t
    | Case_default : { not_in : int list; } -> int t

  (* val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int *)
end

include module type of T with type 'a t = 'a T.t

val of_bool : bool -> bool t
(** [of_bool b] is the boolean direction for [b]. *)

val of_int : int -> int t
(** [of_int i] is the integer case direction on [i]. *)
