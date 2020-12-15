open Core

module T = struct
  type t = Id.t list
  [@@deriving sexp, compare, equal]
end

include T
include Comparator.Make(T)