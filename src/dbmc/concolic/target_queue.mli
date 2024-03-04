(* TODO: consider making this a `Queue` "modifier" inside `Target`. *)

type t
(** [t] is a functional priority queue of targets where pushing a target gives
    it the most priority. If the target was already in the queue, the target is
    moved to the front. *)

val empty : t
(* val is_empty : t -> bool *)
(* val push_one : t -> Target.t -> t *)
val push_list : t -> Target.t list -> t
(** [push_list t ls] pushes all targets in [ls] onto [t], where deeper targets are at the front of [ls] *)
val pop : ?kind:[ `DFS | `BFS | `Random ] -> t -> (Target.t * t) option
(** [pop t] is most prioritized target and new queue, or [None]. *)
(* val to_string : t -> string *)