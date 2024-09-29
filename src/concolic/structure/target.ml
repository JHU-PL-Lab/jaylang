open Core

type t =
  { branch : Branch.Runtime.t (* is included in the path as the deepest direction *)
  ; path_n : int (* this is the length of the path, just so we don't recompute *)
  ; path   : Path.Reverse.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  (* The path is really the only thing that describes the target completely: a series of left and right branch choices *)

let compare (a : t) (b : t) : int =
  match Int.compare a.path_n b.path_n with
  | 0 -> begin
    match Branch.Runtime.compare a.branch b.branch with
    | 0 -> Path.Reverse.compare a.path b.path
    | x -> x
  end 
  | x -> x

let create (branch : Branch.Runtime.t) (path : Path.Reverse.t) : t =
  { path ; path_n = List.length path.backward_path ; branch }
