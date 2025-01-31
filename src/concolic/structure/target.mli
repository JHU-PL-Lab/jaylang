
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
