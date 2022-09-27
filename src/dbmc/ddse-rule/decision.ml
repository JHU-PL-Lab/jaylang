open Core
open Dj_common

module T = struct
  type t = { r_stk : Rstack.t; v : Id.t }
  [@@deriving sexp_of, compare, equal, hash]
end

include T
include Comparator.Make (T)

let make r_stk v = { r_stk; v }
