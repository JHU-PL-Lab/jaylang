
type t =
  { dir    : Direction.Packed.t
  ; path_n : int
  ; path   : Path.Reverse.t }

let compare (a : t) (b : t) : int =
  match Int.compare a.path_n b.path_n with
  | 0 -> begin
    match Direction.Packed.compare a.dir b.dir with
    | 0 -> Path.Reverse.compare a.path b.path
    | x -> x
  end 
  | x -> x