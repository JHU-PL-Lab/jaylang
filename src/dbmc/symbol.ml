open Core

module T = struct
  type t = 
    | Id of Id.t * Relative_stack.t
    (* | Truth *)
  [@@deriving sexp, compare, equal, variants]
end

include T
include Comparator.Make(T)

let pp oc = function
  | Id (x, stk) -> Fmt.(pf oc "%a%a" Id.pp x Relative_stack.pp stk)
(* | Truth -> Fmt.(pf oc "$True") *)

let show = Fmt.to_to_string pp

let id i s = Id(i,s)

(* let to_string_mach t =
   t |> sexp_of_t |> Sexp.to_string_mach *)