open Core

type t =
  { branch : Branch.Runtime.t [@compare.ignore] (* is included in the path, so we don't need to compare *)
  ; path_n : int [@compare.ignore] (* this is the length of the path, just so we don't recompute *)
  ; path   : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
  [@@deriving compare]  
  (* The path is really the only thing that describes the target completely: a series of left and right branch choices *)

let compare (a : t) (b : t) : int =
  match Int.compare a.path_n b.path_n with
  | 0 -> compare a b (* call the derived compare *)
  | x -> x

let create (branch : Branch.Runtime.t) (path : Path.t) : t =
  { path ; path_n = List.length path.forward_path ; branch }

(* TODO: get rid of this *)
let dudd : t =
  let dudd_key = Concolic_key.create (Jayil.Ast.Ident "") 0 in
  { branch = { branch_key = dudd_key ; condition_key = dudd_key ; direction = True_direction }
  ; path = Path.empty
  ; path_n = 0 }