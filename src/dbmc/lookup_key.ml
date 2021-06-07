open Core

module T = struct
  type t = Id.t * Lookup_stack.t * Relative_stack.t
  [@@deriving sexp, compare, equal, show { with_path = false }]

  let hash = Hashtbl.hash
end

include T
include Comparator.Make (T)
