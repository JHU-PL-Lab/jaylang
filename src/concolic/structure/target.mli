(**
  File: target.mli
  Purpose: represent the target of solves

  Detailed description:
    The concolic evaluator solves to hit program paths, but sometimes
    we want a little extra information than just the path.

    Targets do that, storing a unique identifier and the length of the
    path to avoid recomputation.

    SUPER IMPORTANT NOTE:
      It is an invariant in this implementation of concolic evaluation
      that each target is only created once, and therefore we use an
      internal state to attach a unique identifier to each target for
      efficient comparison later (since they are stored in priority
      search queues, they get compared a lot).

      This will break if the concolic evaluator does not have this
      property, and it won't break loudly, so the developer must be
      very careful that this assumption continues to hold. That is, 
      the entire concolic evaluation system will be very quietly
      incorrect if this property is violated.

  Dependencies:
    Path -- targets are really just paths with some precomputation when creating
    Direction -- implicitly because of path
*)

type t

val make : Path.Reverse.t -> t

val compare : t -> t -> int

val to_rev_path : t -> Path.Reverse.t
(** [to_rev_path target] is the reverse path to the target in constant time. *)

val to_path : t -> Path.t
(** [to_path target] is the path to the target in linear time. *)

val append_path : Path.t -> t -> t

val dir : t -> Direction.Packed.t
(** [dir target] is the final direction of the [target] *)

val path_n : t -> int
(** [path_n target] is the length of the patch to the [target]. *)
