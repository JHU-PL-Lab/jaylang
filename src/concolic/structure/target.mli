
type t =
  { branch : Branch.Runtime.t (* this branch is the last in the path *)
  ; path_n : int
  ; path   : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  (* The path is really the only thing that describes the target completely: a series of left and right branch choices *)

val compare : t -> t -> int
(** [compare a b] compares the paths in the targets [a] and [b], and it preliminarily uses the branch and path length to weed out similar paths. *)

val create : Branch.Runtime.t -> Path.t -> t
(** [create branch path] is a target for the [branch] and the end of the [path], where [branch] is included as the last
    element in the [path]. *)
