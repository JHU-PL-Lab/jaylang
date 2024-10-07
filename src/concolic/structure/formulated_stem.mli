
(* TODO : update docstrings *)

type t =
  | Root of { expr_cache : Expression.Cache.t }
  | Cons of { branch : Branch.Runtime.t ; expr_cache : Expression.Cache.t ; tail : t }

val empty : t
(** [empty] is a stem with no formulas at all. *)

val of_cache : Expression.Cache.t -> t
(* val of_target : Target.t -> t *)
(** [of_target target] has a formula that requires the [target] be hit. *)

val push_expr : 'a. t -> Concolic_key.t -> 'a Expression.t -> t

val push_alias : t -> Concolic_key.t -> Concolic_key.t -> t

val push_branch : t -> Branch.Runtime.t -> t
(** [push_branch t branch] is cons with [t] and has a formula that requires [branch] be hit. *)

val binop : t -> Concolic_key.t -> Expression.Untyped_binop.t -> Concolic_key.t -> Concolic_key.t -> t

val not_ : t -> Concolic_key.t -> Concolic_key.t -> t

val is_const_bool : t -> Concolic_key.t -> bool

(* val push_formula : t -> Z3.Expr.expr -> t *)
(** [push_formula t expr] adds [expr] to [t] in the current scope. *)

val to_rev_path : t -> Path.Reverse.t
(** [to_rev_path t] is the reverse path to the final branch in [t]. *)

