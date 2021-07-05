open Core

module T = struct
  type t = Id.t * Lookup_stack.t * Relative_stack.t
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]
end

include T
include Comparator.Make (T)

let get_lookups key =
  let x, xs, _ = key in
  x :: xs
