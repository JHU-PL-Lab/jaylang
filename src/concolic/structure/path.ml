
type t = bool Expression.t list * int

let empty : t = [], 0

let cons (e : bool Expression.t) (l, n : t) : t =
  e :: l, n + 1

let to_exprs (l, _ : t) : bool Expression.t list =
  l

let length (_, n : t) : int =
  n
