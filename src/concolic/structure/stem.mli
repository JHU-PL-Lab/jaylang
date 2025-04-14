(**
  File: stem.mli
  Purpose: store the results of interpretation to be added to the path tree

  Detailed description:
    An interpretation tends to have a target (or it is the first run and just
    begins from the program root), and once that target is found, we start
    remembering the branches encountered. This way, we can extend the path
    tree with all the new information, while everything encountered *before*
    we hit the target was already known. We don't *literally* extend a path
    tree because such information is not needed, but that is what is happening
    behind the algorithm.

  Dependencies:
    Target -- stems tend to begin from a target, extending the target's path
    Direction -- stems are built by pushing branches
    Expression -- stems remember the symbolic constraints of the path taken
*)

type t

val empty : t
(** [empty] is [Root] *)

val of_target : Target.t -> t
(** [of_target target] is a stem beginning from [target]. *)

val push_branch : t -> bool Direction.t -> bool Expression.t -> t
(** [push_branch stem direction expression] has branched off of [stem] with the claim
    that the [expression] takes the [direction]. *)

val push_case : t -> int Direction.t -> int Expression.t -> int list -> t
(** [push_branch stem direction expression other_int_cases] has branched off of [stem] with the claim
    that the [expression] takes the [direction], where all of the [other_int_cases] are given so that
    the default direction can be computed if needed. *)

val to_new_targets : t -> Target.t list
(** [to_new_targets stem] are all of the targets along the stem that have never been created and
    should be pushed to a queue to hit in future concolic runs. *)
