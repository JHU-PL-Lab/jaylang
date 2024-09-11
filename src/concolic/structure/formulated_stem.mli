
type t =
  | Root of { root_formulas : Formula_set.t }
  | Cons of { branch : Branch.Runtime.t ; formulas : Formula_set.t ; tail : t }

val empty : t
(** [empty] is a stem with no formulas at all. *)

val of_target : Target.t -> t
(** [of_target target] has a formula that requires the [target] be hit. *)

val push_branch : t -> Branch.Runtime.t -> t
(** [push_branch t branch] is cons with [t] and has a formula that requires [branch] be hit. *)

val push_formula : t -> Z3.Expr.expr -> t
(** [push_formula t expr] adds [expr] to [t] in the current scope. *)

val to_path : t -> Path.t
(** [to_path t] is a path to the final branch in [t]. *)
