
type t =
  | Root of { expr_cache : Expression.Cache.t }
  | Cons of { branch : Branch.Runtime.t ; expr_cache : Expression.Cache.t ; tail : t }

val empty : t
(** [empty] is a stem with no formulas at all. *)

val of_cache : Expression.Cache.t -> t
(** [of_cache cache] is a stem that has started with the given [cache] at its root. *)

val push_expr : 'a. t -> Concolic_key.t -> 'a Expression.t -> t
(** [push_expr stem key expr] is the [stem] with the [key] mapping to [expr] pushed to its current cache. *)

val push_alias : t -> Concolic_key.t -> Concolic_key.t -> t
(** [push_expr stem x y] is the [stem] with its cache having that [x] is now an alias for [y] (where [y] existed first). *)

val push_branch : t -> Branch.Runtime.t -> t
(** [push_branch stem branch] is the [stem] with a new most recent [branch]. *)

val binop : t -> Concolic_key.t -> Expression.Untyped_binop.t -> Concolic_key.t -> Concolic_key.t -> t
(** [binop stem x op y z] is the [stem] with its cache having [x = y op z]. *)

val not_ : t -> Concolic_key.t -> Concolic_key.t -> t
(** [not_ stem x y] is [stem] with its cache having [x = not y]. *)

val is_const_bool : t -> Concolic_key.t -> bool
(** [is_const_bool stem key] is false if and only if [key] maps to an abstract expression. *)

val to_rev_path : t -> Path.Reverse.t
(** [to_rev_path t] is the reverse path to the final branch in [t]. *)

