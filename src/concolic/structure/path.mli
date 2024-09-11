
type t = { forward_path : Branch.Direction.t list }

val compare : t -> t -> int

val empty : t
(** [empty] is a path with no directions. *)

val return : Branch.Direction.t list -> t
(** [return ls] is a path of the forward direction list [ls]. *)

val append : t -> Branch.Direction.t -> t
(** [append t dir] is a path with [dir] appended as the last direction to [t.forward_path]. *)

val concat : t -> t -> t
(** [concat a b] is the path [a] followed by [b].  *)

val drop_last_exn : t -> t
(** [drop_last_exn t] is a path with all but the last direction in [t], or exception if [t] is empty. *)
