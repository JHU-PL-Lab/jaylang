open Core

module T = struct
  type t = Id.t * Concrete_stack.t
  [@@deriving sexp_of, compare, equal, hash, show]
end

include T
(* include Comparator.Make (T) *)

let show (x, stk) = Id.show x ^ "@" ^ Concrete_stack.to_string stk
let id_of (x, _) = x
let stack_of (_, stk) = stk
