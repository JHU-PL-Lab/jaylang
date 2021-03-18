open Core

module T = struct
  type t = Lookup_stack.t * Id.t * Relative_stack.t
  [@@deriving sexp, compare, equal, show {with_path = false}]
end

include T
include Comparator.Make(T)