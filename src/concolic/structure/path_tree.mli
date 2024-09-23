
type t
(** [t] is a tree that will hold all program paths *)

(* val empty : t
* [empty] knows no program paths yet. *)

val of_options : (unit, t) Options.Fun.a
(** [of_options ()] take optional arguments to set target queue options, and it knows no program paths yet. *)

val formulas_of_target : t -> Target.t -> Z3.Expr.expr list
(** [formulas_of_target t target] are the formulas required to hit the [target] in the path tree [t] *)

val of_stem : (Formulated_stem.t,  bool -> Branch.t list -> t) Options.Fun.a
(** [of_stem stem failed_assume] is a function with optional concolic arguments that returns the  tree
    that is made entirely from one stem, where the stem was made from an interpretation that failed an
    assume/assert iff [failed_assume] is true. *)

val add_stem : t -> Target.t -> Formulated_stem.t -> bool -> Branch.t list -> t
(** [add_stem t old_target stem failed_assume] adds the [stem] to the path tree [t] beginning from the
    [old_target], which was hit at the root of the stem. The interpretation that generated the [stem]
    ended in a failed assume/assert iff [failed_assume] is true. *)

val set_unsat_target : t -> Target.t -> t
(** [set_unsat_target t target] is [t] where the given [target] has been marked off as unsatisfiable. *)

val pop_target : ?kind:Target_queue.Pop_kind.t -> t -> (Target.t * t) option
(** [pop_target t] is most prioritized target and new queue, or [None] if there are no targets let.
    Default kind is [DFS] *)