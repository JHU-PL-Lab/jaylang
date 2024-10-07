
(* TODO : update docstrings *)

type t =
  | Root of { expr_cache : Expression.Cache.t }
  | Cons of { branch : Branch.Runtime.t ; expr_cache : Expression.Cache.t ; tail : t }

val empty : t
(** [empty] is a stem with no formulas at all. *)

val of_cache : Expression.Cache.t -> t

val push_expr : 'a. t -> Concolic_key.t -> 'a Expression.t -> t

val push_alias : t -> Concolic_key.t -> Concolic_key.t -> t

val push_branch : t -> Branch.Runtime.t -> t

val binop : t -> Concolic_key.t -> Expression.Untyped_binop.t -> Concolic_key.t -> Concolic_key.t -> t

val not_ : t -> Concolic_key.t -> Concolic_key.t -> t

val is_const_bool : t -> Concolic_key.t -> bool

val to_rev_path : t -> Path.Reverse.t
(** [to_rev_path t] is the reverse path to the final branch in [t]. *)

