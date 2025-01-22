
type t =
  { branch : Branch.t
  ; path_n : int
  ; path   : Path.Reverse.t }

let compare (a : t) (b : t) : int =
  match Int.compare a.path_n b.path_n with
  | 0 -> begin
    match Branch.compare a.branch b.branch with
    | 0 -> Path.Reverse.compare a.path b.path
    | x -> x
  end 
  | x -> x