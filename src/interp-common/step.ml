
open Core

module T = struct
  type t = Step of int [@@deriving compare, equal, sexp]
    [@@unboxed]

  let zero = Step 0

  let next (Step i) = Step (i + 1)

  let to_int (Step i) = i

  let uid = to_int

  let to_string (Step i) = Int.to_string i
end

include T

module Map = Map.Make (T)
module Set = Set.Make (T)
