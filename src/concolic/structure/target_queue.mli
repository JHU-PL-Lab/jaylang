
module Pop_kind : sig
  type t =
    | DFS
    | BFS
    | Uniform
    | Random
end

type t
(** [t] is a functional priority search queue of targets that can be popped according
    to the pop kind. *)

val empty : t

val of_options : (unit, t) Options.Fun.a

val push_list : t -> Target.t list -> t
(** [push_list t ls] pushes all targets in [ls] (which have no necessary order in the list) onto [t] *)

val pop : ?kind:Pop_kind.t -> t -> (Target.t * t) option
(** [pop ~kind t] is most prioritized target and new queue, or [None] if empty. [kind] is random of all kinds
    that exist in [t] if unspecified. Raises an exception if [kind] is not valid. *)
