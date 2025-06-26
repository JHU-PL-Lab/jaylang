
open Core

(* TODO: will want to hashcons *)
module T = struct
  type t = Cstack of Lang.Ast.Program_point.t list [@@unboxed]
    [@@deriving compare]
end

include T

let zero = Cstack []

let cons p (Cstack cstack) = Cstack (p :: cstack)

module Map = Baby.W.Map.Make (T)
