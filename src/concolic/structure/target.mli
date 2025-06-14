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

    Note that because targets are only built using `cons`, the expressions
    are shared, and all of the targets built effectively create a path tree
    that is spread quite randomly across memory. It is conjectured, then, that
    it would be no more efficient to store a path tree to explicitly share the
    expressions than to let them be shared this way. To summarize: all targets
    implicitly build a path tree and share expressions along shared paths.


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
    Claim -- is built using claims
    Expression -- can be turned into the expressions needed to solve for it
*)

type t

val make : int -> Path.t -> t

val step : t -> int

(* val empty : t *)
(** [empty] is the meaningless target. It targets nothing. *)

(* val cons : 'a Claim.t -> t -> t *)
(** [cons claim t] is a new target where the [claim] is pushed on top
    of [t] as another constraint. *)

val compare : t -> t -> int
(** [compare a b] uses the unique identifiers in [a] and [b] to compare,
    and hence only literal equality (of memory location) is sufficient
    for [compare a b] to be [0]. *)

val to_expressions : t -> bool Expression.t list
(** [to_expressions t] are the constraints to solve in order to realize [t]. *)

val path_n : t -> int
(** [path_n target] is the length of the path to the [target].
    [path_n empty] is [0]. *)
