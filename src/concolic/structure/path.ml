(*
  A path is a sequence of nodes in a tree. The only thing that really matters is the direction of branches taken.
*)

open Core

module T =
  struct
    (* Branches at the top of the tree are first *)
    type t = { forward_path : Branch.Direction.t list }

    let compare a b = Core.List.compare Branch.Direction.compare a.forward_path b.forward_path

    let empty = { forward_path = [] }

    let return ls = { forward_path = ls }

    let extend path tail =
      { forward_path = path.forward_path @ tail }

    let append path last =
      extend path [ last ]

    let concat path1 path2 = 
      extend path1 path2.forward_path

    let drop_last_exn path =
      return
      @@ List.drop_last_exn path.forward_path

    let tl_exn path =
      return
      @@ List.tl_exn path.forward_path
  end

include T

module Reverse =
  struct
    type t = { reverse_path : Branch.Direction.t list }

    let compare a b = Core.List.compare Branch.Direction.compare a.reverse_path b.reverse_path
  end
