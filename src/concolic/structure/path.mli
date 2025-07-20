
type t
(** [t] represents symbolic path constraints during concolic evaluation. *)

val empty : t
(** [empty] is the empty path. *)

val cons : bool Formula.t -> t -> t
(** [cons e p] is a path with [e] on the front of [p]. *)

val to_exprs : t -> bool Formula.t list
(** [to_exprs p] is all of the expressions in the path [p]. *)

val length : t -> int
(** [length p] is the length of [p]. This function takes constant time. *)
