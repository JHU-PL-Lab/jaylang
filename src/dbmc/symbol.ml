open Core

module T = struct
  type t = Id of Id.t * Rstack.t [@@deriving sexp, compare, equal, variants]
end

include T
include Comparator.Make (T)

let pp _oc = failwith "not used"

let show : t -> string = Fmt.to_to_string pp

let id i s = Id (i, s)

let name_of_lookup xs stk =
  match xs with
  | _ :: _ ->
      let p1 = Lookup_stack.mk_name xs in
      let p2 = Rstack.str_of_t stk in
      p1 ^ p2
  | [] -> ""
