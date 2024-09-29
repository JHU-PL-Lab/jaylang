
open Core

(* Items earlier in cons are those that are deeper in a path tree *)
type t =
  | Root of { root_formulas : Formula_set.t }
  | Cons of { branch : Branch.Runtime.t ; formulas : Formula_set.t ; tail : t }

let empty : t = Root { root_formulas = Formula_set.empty }

let of_target (target : Target.t) : t =
  Root { root_formulas = Formula_set.singleton @@ Branch.Runtime.to_expr target.branch }

let push_branch (tail : t) (branch : Branch.Runtime.t) : t =
  Cons { branch ; formulas = Formula_set.singleton @@ Branch.Runtime.to_expr branch ; tail }

let push_formula (x : t) (expr : Z3.Expr.expr) : t =
  match x with
  | Root { root_formulas } -> Root { root_formulas = Formula_set.add root_formulas expr }
  | Cons r -> Cons { r with formulas = Formula_set.add r.formulas expr }

let to_rev_path (x : t) : Path.Reverse.t =
  let rec loop acc = function
    | Root _ -> acc
    | Cons { branch = { direction ; _ } ; tail ; _ } -> loop (direction :: acc) tail
  in
  loop [] x
  |> List.rev
  |> Path.Reverse.return
