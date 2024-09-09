module Pop_kind :
  sig
    type t =
      | DFS
      | BFS
      | Uniform
      | Random
  end

type t
(** [t] is a functional priority queue of targets where pushing a target gives
    it the most priority. If the target was already in the queue, the target is
    moved to the front. *)

val empty : t

val push_list : t -> Target.t list -> t
(** [push_list t ls] pushes all targets in [ls] onto [t], where deeper targets are at the back of [ls] *)

val with_options : (t, t) Options.Fun.t

val pop : ?kind:Pop_kind.t -> t -> (Target.t * t) option
(** [pop t] is most prioritized target and new queue, or [None]. Default kind is [DFS] *)