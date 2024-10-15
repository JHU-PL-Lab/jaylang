module Pop_kind :
  sig
    type t =
      | DFS
      | BFS
      | Prioritize_uncovered
      | Uniform
      | Random
  end

type t
(** [t] is a functional priority queue of targets where pushing a target gives
    it the most priority. If the target was already in the queue, the target is
    moved to the front. *)

val empty : t
val push_list : t -> Target.t list -> int list -> t
(** [push_list t ls hits] pushes all targets in [ls] onto [t], where deeper targets are at the front of [ls],
    and [hits] are the number of times each target in [ls] has been hit in the AST. *)
val with_options : (t -> t) Options.Fun.t
val pop : ?kind:Pop_kind.t -> t -> (Target.t * t) option
(** [pop t] is most prioritized target and new queue, or [None]. Default kind is [DFS] *)