
open Core

module T = struct
  type t = Step of int [@@deriving compare, equal, sexp]
    [@@unboxed]

  let zero = Step 0

  let next (Step i) = Step (i + 1)

  let to_int (Step i) = i

  let to_string (Step i) = Int.to_string i
end

include T

module Map = Map.Make (T)
module Set = Set.Make (T)

module With_max (X : sig val max_step : int end) = struct
  include T
  let exceeds_max (Step i) = i > X.max_step
  let max_step = Step X.max_step
end
