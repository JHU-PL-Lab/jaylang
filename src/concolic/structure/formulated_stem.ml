
open Core

(* Items earlier in cons are those that are deeper in a path tree *)
type t =
  | Root of { expr_cache : Expression.Cache.t }
  | Cons of { branch : Branch.Runtime.t ; expr_cache : Expression.Cache.t ; tail : t }

let empty : t = Root { expr_cache = Expression.Cache.empty }

let expr_cache = function
  | Root { expr_cache }
  | Cons { expr_cache ; _ } -> expr_cache

let of_cache expr_cache = Root { expr_cache }

(* let of_target (target : Target.t) : t =
  Root { root_formulas = Formula_set.singleton @@ Branch.Runtime.to_expr target.branch } *)

let push_branch (tail : t) (branch : Branch.Runtime.t) : t =
  Cons { branch ; expr_cache = expr_cache tail ; tail }

let push_expr (type a) (x : t) (key : Concolic_key.t) (expr : a Expression.t) : t =
  match x with
  | Root { expr_cache } -> Root { expr_cache = Expression.Cache.add_expr expr_cache key expr }
  | Cons r -> Cons { r with expr_cache = Expression.Cache.add_expr r.expr_cache key expr }

let push_alias (x : t) (key1 : Concolic_key.t) (key2 : Concolic_key.t) : t =
  match x with
  | Root { expr_cache } -> Root { expr_cache = Expression.Cache.add_alias key1 key2 expr_cache }
  | Cons r -> Cons { r with expr_cache = Expression.Cache.add_alias key1 key2 r.expr_cache }

(* let push_expr (x : t) (expr : ) : t =
  match x with
  | Root { root_formulas } -> Root { root_formulas = Formula_set.add root_formulas expr }
  | Cons r -> Cons { r with formulas = Formula_set.add r.formulas expr } *)

let to_rev_path (x : t) : Path.Reverse.t =
  let rec loop acc = function
    | Root _ -> acc
    | Cons { branch = { direction ; _ } ; tail ; _ } -> loop (direction :: acc) tail
  in
  loop [] x
  |> List.rev
  |> Path.Reverse.return
