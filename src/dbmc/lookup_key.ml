open Core

module T = struct
  type t = { x : Id.t; xs : Lookup_stack.t; r_stk : Relative_stack.t }
  [@@deriving sexp, compare, equal, hash, show { with_path = false }]
end

include T
include Comparator.Make (T)

let start (x : Id.t) : t = { x; xs = []; r_stk = Relative_stack.empty }

let of_parts x xs r_stk = { x; xs; r_stk }

let of_parts2 xs r_stk = { x = List.hd_exn xs; xs = List.tl_exn xs; r_stk }

let to_parts key = (key.x, key.xs, key.r_stk)

let to_first key x = { key with x; xs = [] }

let replace_x key x = { key with x }

let drop_x key = { key with x = List.hd_exn key.xs; xs = List.tl_exn key.xs }

let lookups key = key.x :: key.xs

let get_lookups key =
  let x, xs, _ = key in
  x :: xs
