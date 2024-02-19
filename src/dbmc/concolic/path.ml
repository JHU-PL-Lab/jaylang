(* A path is a sequence of nodes in a tree. The only thing that really matters is the branches taken *)

module T =
  struct
    (* Branches at the top of the tree are first *)
    type t = Branch.Runtime.t list

    let compare = Core.List.compare Branch.Runtime.compare
  end

include T

module Reverse = T (* This is just for annotating via types *)