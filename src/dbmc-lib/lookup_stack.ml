open Core

module T = struct
  type t = Id.t list [@@deriving sexp, compare, equal, show {with_path = false}]
end

include T
include Comparator.Make(T)