(*
  A path is a sequence of nodes in a tree. The only thing that really matters is the direction of branches taken.
*)

open Core

module T =
struct
  (* Branches at the top of the tree are first *)
  type t = { forward_path : Branch.Direction.t list }
end

include T

module Reverse =
struct
  (* Branches at the front are the leaves of the tree *)
  type t = { backward_path : Branch.Direction.t list }

  let compare a b = Core.List.compare Branch.Direction.compare a.backward_path b.backward_path

  let empty : t = { backward_path = [] }

  let return ls = { backward_path = ls }

  let cons front path =
    return
    @@ front :: path.backward_path

  let concat path1 path2 = 
    return 
    @@ path1.backward_path @ path2.backward_path

  let drop_hd_exn path =
    return
    @@ List.tl_exn path.backward_path

  let to_forward_path x =
    { forward_path = List.rev x.backward_path }
  
end
