open Core

module T = struct
  type t = Id of Id.t * Rstack.t [@@deriving sexp, compare, equal, variants]
end

include T
include Comparator.Make (T)

let pp oc = function Id (x, stk) -> Fmt.(pf oc "%a%a" Id.pp x Rstack.pp stk)

let show = Fmt.to_to_string pp

let id i s = Id (i, s)
