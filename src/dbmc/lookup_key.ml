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

let str_of_id key =
  Printf.sprintf "%s%s"
    (Lookup_stack.str_of_t (lookups key))
    (Rstack.str_of_id key.r_stk)

let pp_id = Fmt.of_to_string str_of_id

let chrono_compare map k1 k2 =
  let x1, xs1, r_stk1 = to_parts k1 in
  let x2, xs2, r_stk2 = to_parts k2 in
  assert (List.is_empty xs1);
  assert (List.is_empty xs2);
  let result = Rstack.compare r_stk1 r_stk2 in
  if result = 0 then
    if Tracelet.is_before map x1 x2 then
      1
    else
      -1
  else
    result
