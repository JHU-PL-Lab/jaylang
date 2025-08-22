
type 'k t
(** ['k t] is a concolic path, where symbolic formulas are keyed by ['k]. *)

val empty : 'k t
(** [empty] is the path of length [0]. *)

val cons : 'k Direction.t -> 'k t -> 'k t
(** [cons dir path] puts [dir] as the most recent direction taken after [path]. *)

val to_dirs : 'k t -> 'k Direction.t list
(** [to_dirs path] is the list of directions in the order they were consed onto [path]. *)

val length : 'k t -> int
(** [length path] is the number of directions in [path], in constant time. *)
