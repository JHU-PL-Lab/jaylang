
open Core

type t = Fun_depth of Int.t
[@@deriving hash, compare, equal, sexp]

let incr (Fun_depth i) = Fun_depth (i + 1)

let zero = Fun_depth 0

let to_int (Fun_depth i) = i

let to_string fd = Int.to_string @@ to_int fd
