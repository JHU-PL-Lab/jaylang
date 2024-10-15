(* A path is a sequence of nodes in a tree. The only thing that really matters is the branches taken *)

module T =
  struct
    (* Branches at the top of the tree are first *)
    type t = { forward_path : Branch.Runtime.t list }

    let compare a b = Core.List.compare Branch.Runtime.compare a.forward_path b.forward_path

    let empty = { forward_path = [] }

    let return ls = { forward_path = ls }

    let extend path tail =
      { forward_path = path.forward_path @ tail }
  end

include T

module Reverse =
  struct
    type t = { reverse_path : Branch.Runtime.t list }

    let compare a b = Core.List.compare Branch.Runtime.compare a.reverse_path b.reverse_path
  end
