
open Core

type t = 
  | Callstack of Lang.Ast.Program_point.t list [@@unboxed]
  [@@deriving compare, equal]

let to_string : t -> string = function
  | Callstack ls ->
    String.concat ~sep:"." (List.map ls ~f:(fun (Lang.Ast.Program_point.Program_point i) -> Int.to_string i))

let empty = Callstack []

let cons p (Callstack cstack) = Callstack (p :: cstack)