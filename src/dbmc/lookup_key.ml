open Core

module T = struct
  type t = { x : Id.t; xs : Lookup_stack.t; r_stk : Rstack.t }
  [@@deriving sexp, compare, equal, hash]
end

include T
include Comparator.Make (T)

let start (x : Id.t) : t = { x; xs = []; r_stk = Rstack.empty }

let of_parts x xs r_stk = { x; xs; r_stk }

let of_parts2 xs r_stk = { x = List.hd_exn xs; xs = List.tl_exn xs; r_stk }

let to_parts key = (key.x, key.xs, key.r_stk)

let to_parts2 key = (key.x :: key.xs, key.r_stk)

let to_first key x = { key with x; xs = [] }

let replace_x key x = { key with x }

let drop_x key = { key with x = List.hd_exn key.xs; xs = List.tl_exn key.xs }

let lookups key = key.x :: key.xs

let to_str key =
  Printf.sprintf "%s%s"
    (Lookup_stack.str_of_t (lookups key))
    (Rstack.str_of_t key.r_stk)

let parts_to_str x xs r_stk = to_str (of_parts x xs r_stk)

let parts2_to_str xs r_stk = to_str (of_parts2 xs r_stk)

let show = to_str

let pp = Fmt.of_to_string show
