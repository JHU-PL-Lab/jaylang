(**
  SUPER IMPORTANT NOTE:
    It is an invariant in this implementation of concolic evaluation
    that each target is only created once, and therefore we use an
    internal state to attach a unique identifier to each target for
    efficient comparison later (since they are stored in priority
    search queues, they get compared a lot).

    This will break if the concolic evaluator does not have this
    property, and it won't break loudly, so the developer must be
    very careful that this assumption continues to hold. That is, 
    the entire concolic evaluation system will be quietly incorrect
    if this property is violated.
*)

type 'k t

val empty : 'k t

val cons : (bool, 'k) Smt.Formula.t -> 'k t -> 'k t

val compare : 'k t -> 'k t -> int
(** [compare a b] uses the unique identifiers in [a] and [b] to compare,
    and hence only literal equality (of memory location) is sufficient
    for [compare a b] to be [0]. *)

val to_expressions : 'k t -> (bool, 'k) Smt.Formula.t list
(** [to_expressions t] are the constraints to solve in order to realize [t]. *)

val path_n : 'k t -> int
(** [path_n target] is the length of the path to the [target]. *)
