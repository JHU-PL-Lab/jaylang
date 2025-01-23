
type t =
  { dir    : Direction.Packed.t
  ; path_n : int
  ; path   : Path.Reverse.t }

val make : Path.Reverse.t -> t

val compare : t -> t -> int

val to_path : t -> Path.t

val append_path : Path.t -> t -> t
