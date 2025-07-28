
type 'k t = 'k Direction.t list * int

let empty : 'k t = [], 0

let cons (e : 'k Direction.t) (l, n : 'k t) : 'k t =
  e :: l, n + 1

let to_dirs (l, _ : 'k t) : 'k Direction.t list =
  List.rev l

let length (_, n : 'k t) : int =
  n
