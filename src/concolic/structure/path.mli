(**
  File: path.mli
  Purpose: represent program paths

  Detailed description:
    Program paths are just a list of directions taken during interpretation.
    Most of the time, it is most efficient to store them bottom-up as a
    reverse path.

    Forward paths are top down with respect to the tree; branches that are
    found earlier during interpretation are earlier in the list of directions.

    Backward paths are the opposite. The last branch hit is stored at the front
    of the list.

  Dependencies:
    Direction -- paths are a list of directions
*)

module T : sig 
  type t = { forward_path : Direction.Packed.t list }
    [@@unboxed][@@deriving compare]
end

type t = T.t

val empty : t

module Reverse : sig 
  type t = { backward_path : Direction.Packed.t list }
    [@@unboxed][@@deriving compare]

  val compare : t -> t -> int

  val empty : t
  (** [empty] is a path with no directions. *)

  val return : Direction.Packed.t list -> t
  (** [return ls] is a path of the reverse direction list [ls]. *)

  val cons : Direction.Packed.t -> t -> t
  (** [cons dir t] is a path with [dir] put on the front of [t.backward_path]. *)

  val concat : t -> t -> t
  (** [concat a b] is the reverse path [a.backward_path @ b.backward_path].*)

  val drop_hd_exn : t -> t
  (** [drop_hd_exn t] is [t] with the head of [t.backward_path]. *)

  val to_forward_path : t -> T.t
  (** [to_forward_path t] is the reversed list in [t] as a forward path. *)

  val of_forward_path : T.t -> t
end