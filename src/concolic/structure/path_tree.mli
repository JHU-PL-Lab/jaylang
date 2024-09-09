
type t
(** [t] is a tree that will hold all program paths *)

val empty : t
(** [empty] knows no program paths yet. *)

val formulas_of_target : t -> Target.t -> Z3.Expr.expr list
(** [formulas_of_target t target] are the formulas required to hit the [target] in the path tree [t] *)

val add_stem : t -> Target.t -> Formulated_stem.t -> bool -> t * Target.t list
(** [add_stem t old_target stem failed_assume] adds the [stem] to the path tree [t] beginning from the
    [old_target], which was hit at the root of the stem. The interpretation that generated the [stem]
    ended in a failed assume/assert iff [failed_assume] is true.
      
    The new path tree and the acquired targets are returned. *)

val set_unsat_target : t -> Target.t -> t
(** [set_unsat_target t target] is [t] where the given [target] has been marked off as unsatisfiable. *)