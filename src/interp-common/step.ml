
open Core

module T = struct
  type t = Step of int [@@unboxed] [@@deriving compare, equal, sexp]

  let zero = Step 0

  let[@inline always][@specialize] next (Step i) = Step (i + 1)

  let[@inline always][@specialize] to_int (Step i) = i

  let uid = to_int

  let to_string (Step i) = Int.to_string i
end

include T

module Map = Map.Make (T)
module Set = Set.Make (T)
