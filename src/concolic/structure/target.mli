(**
  File: target.mli
  Purpose: represent the target of solves

  Detailed description:
    The concolic evaluator solves to hit program paths, but sometimes
    we want a little extra information than just the path.

    In fact, we don't even need to store the path if we're not maintaining
    a path tree, and only the expressions needed to solve for the target
    are needed. This means that it is difficult to assert that the target
    was hit because there is no information about what that target *is*,
    but over several iterations of this system (many of which allowed such
    assertions), we feel sure about correctness.

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

  Dependencies:
    Path -- the target is built upon a path
    Expression -- can be turned into the expressions needed to solve for it
*)

type t

val make : step:int -> Path.t -> t
(** [make step path] is a target that will be encountered after [~step] many
    interpretation steps, and it conforms to the given [path] constraints.
    It has a fresh, unique identifier that is used for comparison. *)

val step : t -> int
(** [step target] is the step passed into the target at creation. *)

val compare : t -> t -> int
(** [compare a b] uses the unique identifiers in [a] and [b] to compare,
    and hence only literal equality (of memory location) is sufficient
    for [compare a b] to be [0]. *)

val to_expressions : t -> bool Expression.t list
(** [to_expressions t] are the constraints to solve in order to realize [t]. *)

val path_n : t -> int
(** [path_n target] is the length of the path to the [target]. *)
