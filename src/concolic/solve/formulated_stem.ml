
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

let map_expr_cache (x : t) (f : Expression.Cache.t -> Expression.Cache.t) : t =
  match x with
  | Root { expr_cache } -> Root { expr_cache = f expr_cache }
  | Cons r -> Cons { r with expr_cache = f r.expr_cache }

let push_branch (tail : t) (branch : Branch.Runtime.t) : t =
  Cons { branch ; expr_cache = expr_cache tail ; tail }

let push_expr (type a) (x : t) (key : Concolic_key.t) (expr : a Expression.t) : t =
  map_expr_cache x @@ fun expr_cache ->
    Expression.Cache.add_expr expr_cache key expr

let push_alias (x : t) (key1 : Concolic_key.t) (key2 : Concolic_key.t) : t =
  map_expr_cache x @@ fun expr_cache ->
    Expression.Cache.add_alias key1 key2 expr_cache

let binop (x : t) (key : Concolic_key.t) (untyped_binop : Expression.Untyped_binop.t) (left : Concolic_key.t) (right : Concolic_key.t) : t =
  map_expr_cache x @@ fun expr_cache ->
    Expression.Cache.binop key untyped_binop left right expr_cache

let not_ (x : t) (key1 : Concolic_key.t) (key2 : Concolic_key.t) : t =
  map_expr_cache x @@ fun expr_cache ->
    key2
    |> Expression.Cache.lookup_bool expr_cache
    |> Expression.not_
    |> Expression.Cache.add_expr expr_cache key1

let is_const_bool (x : t) (key : Concolic_key.t) : bool =
  Expression.Cache.is_const_bool (expr_cache x) key

let to_rev_path (x : t) : Path.Reverse.t =
  let rec loop acc = function
    | Root _ -> acc
    | Cons { branch = { direction ; _ } ; tail ; _ } -> loop (direction :: acc) tail
  in
  loop [] x
  |> List.rev
  |> Path.Reverse.return
