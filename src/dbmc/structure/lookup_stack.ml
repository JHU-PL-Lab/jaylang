open Core

module T = struct
  type t = Id.t list
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]
end

include T
include Comparator.Make (T)

let to_string xs = Fmt.(str "%a" (list ~sep:(any ";") Id.pp) xs)
