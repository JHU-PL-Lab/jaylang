
open Core

(* TODO: will want to hashcons *)
module T = struct
  type t = Cstack of Lang.Ast.Program_point.t list [@@unboxed]
    [@@deriving compare]
end

include T

let to_string : t -> string = function
  | Cstack ls ->
    String.concat ~sep:"." (List.map ls ~f:(fun (Lang.Ast.Program_point.Program_point i) -> Int.to_string i))

let zero = Cstack []

let cons p (Cstack cstack) = Cstack (p :: cstack)

module Map = Baby.W.Map.Make (T)
