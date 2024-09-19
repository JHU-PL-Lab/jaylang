module Pop_kind :
  sig
    type t =
      | DFS
      | BFS
      | Uniform
      | By_ast_branch (* prioritizes the targets whose AST branches have been hit the least *)
      | Random
  end

type t
(** [t] is a functional priority search queue of targets that can be popped according
    to the pop kind. *)

(* val make : Pop_kind.t list -> t *)

val of_options : (unit, t) Options.Fun.t

val hit_branches : t -> Branch.t list -> t
(** necessary to call this to keep the By_ast_branch heuristic up to date *)

val push_list : t -> Target.t list -> t
(** [push_list t ls] pushes all targets in [ls] onto [t], where deeper targets are at the back of [ls]. *)
(* TODO: update this to take the list of branches hit. *)

val pop : ?kind:Pop_kind.t -> t -> (Target.t * t) option
(** [pop ~kind t] is most prioritized target and new queue, or [None] if empty. [kind] is random of all kinds
    that exist in [t] if unspecified. Raises an exception if [kind] is not valid. *)
