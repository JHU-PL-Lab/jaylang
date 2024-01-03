(*
  Note: I think if I want to track parents of branches, then I would need
    to track functions as parents because a branch inside a function could
    be underneath many other spots.   
*)

(* type t

val add_runtime_branch : t -> Branch.Runtime.t -> t *)
(** [add_runtime_branch tracker branch] knows that the ast branch underlying
    [branch] has been seen as the runtime branch [branch]. *)

