
type t
(** [t] is a tree that will hold all program paths *)

val of_options : (unit, t) Options.Fun.a
(** [of_options ()] take optional arguments to set target queue options, and it knows no program paths yet. *)

val claims_of_target : t -> Target.t -> Claim.t list * Expression.Cache.t
(** [formulas_of_target t target] are the formulas required to hit the [target] in the path tree [t] *)

val cache_of_target : t -> Target.t -> Expression.Cache.t

val of_stem : (Formulated_stem.t,  Branch.t list -> t) Options.Fun.a
(** [of_stem stem] is a function with optional concolic arguments that returns the tree
    that is made entirely from one stem. *)

val add_stem : t -> Target.t -> Formulated_stem.t -> Branch.t list -> t
(** [add_stem t old_target stem] adds the [stem] to the path tree [t] beginning from the
    [old_target], which was hit at the root of the stem. *)

val set_unsat_target : t -> Target.t -> t
(** [set_unsat_target t target] is [t] where the given [target] has been marked off as unsatisfiable. *)

val set_timeout_target : t -> Target.t -> t
(** [set_timeout_target t target] is [t] where the given [target] has been marked off as causing a solver timeout. *)

val pop_target : ?kind:Target_queue.Pop_kind.t -> t -> (Target.t * t) option
(** [pop_target t] is most prioritized target and new queue, or [None] if there are no targets let.
    Default kind is [DFS] *)