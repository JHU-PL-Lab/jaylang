
open Core

module T = struct
  type t = Step of int [@@deriving compare, equal]
    [@@unboxed]

  let zero = Step 0

  let next (Step i) = Step (i + 1)

  let to_int (Step i) = i
end

include T

module With_max (X : sig val max_step : int end) = struct
  include T
  let exceeds_max (Step i) = i > X.max_step
  let max_step = Step X.max_step
end
