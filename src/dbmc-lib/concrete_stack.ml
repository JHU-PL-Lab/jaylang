open Core
module T = struct
  type frame = Id.t * Id.t
  [@@deriving sexp, compare, equal]
  type t = frame list
  [@@deriving sexp, compare, equal]
end

include T
include Comparator.Make(T)