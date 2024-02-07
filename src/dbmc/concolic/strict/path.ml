(* A path is a sequence of nodes in a tree. The only thing that really matters is the branches taken *)

(* Branches at the top of the tree are first *)
type t = Branch.Runtime.t list