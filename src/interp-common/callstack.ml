
open Core

type t = 
  | Callstack of Lang.Ast.Program_point.t list [@@unboxed]
  [@@deriving equal, sexp]

(* Need to compare back to front because back is oldest *)
(* Could be smart and not reverse, but then we're using stack space.
  I'll just reverse them for now. *)
let compare (Callstack a : t) (Callstack b : t) : int =
  List.compare Lang.Ast.Program_point.compare (List.rev a) (List.rev b)

let to_string : t -> string = function
  | Callstack ls ->
    List.rev ls
    |> List.map ~f:(fun (Lang.Ast.Program_point.Program_point i) -> Int.to_string i)
    |> String.concat ~sep:"."

let empty = Callstack []

let cons p (Callstack cstack) = Callstack (p :: cstack)

(* inefficient for now and uses fixed k  *)
let k_cons : int -> t -> Lang.Ast.Program_point.t -> t = fun k (Callstack stack) callsite ->
  Callstack (List.take (callsite :: stack) k)