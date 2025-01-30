
type t =
  | Int of int
  | Bool of bool
  [@@deriving compare, sexp]
