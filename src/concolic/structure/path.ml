(*
  A path is a sequence of nodes in a tree. The only thing that really matters is the direction of branches taken.
*)

open Core

(* Branches at the top of the tree are first *)
type t = { forward_path : Branch.Direction.t list }

let compare a b = Core.List.compare Branch.Direction.compare a.forward_path b.forward_path

let empty = { forward_path = [] }

let return ls = { forward_path = ls }

let append path last =
  return
  @@ path.forward_path @ [ last ]

let concat path1 path2 = 
  return
  @@ path1.forward_path @ path2.forward_path

let drop_last_exn path =
  return
  @@ List.drop_last_exn path.forward_path
