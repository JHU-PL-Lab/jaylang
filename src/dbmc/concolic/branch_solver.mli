(*
  File: branch_solver.mli
  Purpose: help store information about runtime variables and solve to hit branches.

  Detailed description:
    Values of runtime variables can be added to the solver underneath the parent
    branch as found during the interpretation of the program. Then a branch can
    be picked and solved for, and an input feeder is returned to attempt to hit
    that branch. It's not guaranteed that the inputs will allow the
    interpretation to hit the target branch. Branches that had not yet been hit
    have unknown effects, which can lead to missing the target branch, even if
    all known formulas were satisfied.

  Logic description:
    The solver has a functional state that is the current parent branch: it is the
    global "branch" i.e. environment, or it is some local branch under a condition
    clause.

    Formulas are added underneath this parent, and when exiting the parent, they are
    accumulated and implied by the exited parent and put underneath the new parent.

    When solving, all formulas from the global scope are added to a Z3 solver and
    used to make an input feeder. The Z3 solver is mutable, but the scope of the solver
    is only inside the `get_feeder` function, so to the user, this module is entirely
    functional.
*)

exception NoParentException
(** Used to convey that the user of the solver is trying to add a formula or back up to
    a previous parent when there is no such parent. If the solver is used properly on a
    valid Jayil program, this should be logically impossible. *)

module Formula_set :
  sig
    type t
    (** [t] is a set of Z3 expressions, ie "formulas". *)
    val empty : t
    val add : t -> Z3.Expr.expr -> t
    (** [add set e] adds the expression [e] to the [set]. *)
    val union : t -> t -> t
    (** [union a b] is all formulas in [a] or [b]. *)
    val fold : t -> init:'a -> f:('a -> Z3.Expr.expr -> 'a) -> 'a
    val of_list : Z3.Expr.expr list -> t
  end

type t
(** [t] is a solver that tracks expressions during evaluation of Jayil program. It has a
    functional state that is the current environment of the Jayil program; it is underneath
    some branch in the program. These branches are "if" statements or the global environment. *)

val empty : t
(** [empty] is a solver in the global environment with no formulas. *)

val enter_branch : t -> Branch.Runtime.t -> t
(** [enter_branch s b] puts the solver [s] in the environment under branch [b]. *)

val exit_branch : t -> t
(** [exit_branch s] backs the solver [s] out of its branch into the parent branch. *)

val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val s k v] adds the formula that [k] has value [v] in solver [s]. *)

val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
(** [add_alias s k k'] adds the formula that [k] and [k'] hold the same value in solver [s]. *)

val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
(** [add_binop s x op left right] adds the formula that [x = left op right] to the solver [s]. *)

val add_formula : t -> Z3.Expr.expr -> t (* TODO: hide eventually *)
(** [add_formula s e] adds the z3 expression [e] to the solver [s]. *)

val get_feeder : t -> Branch.Runtime.t -> (Concolic_feeder.t, Branch.Ast_branch.t) result
(** [get_feeder s b] is a feeder to hit the branch [b] based on formulas in the solver [s], 
    or is an error if [b] is unsatisfiable. *)

val get_cur_parent_exn : t -> Branch.Runtime.t
(** [get_cur_parent_exn s] is the branch that defines current environment of the solver [s],
    or is an exception if [s] is in the global environment. *)

val is_global : t -> bool
(** [is_global s] is false if and only if [get_cur_parent_exn s] throws an exception. *)

val add_formula_set : Formula_set.t -> t -> t
(** [add_formula_set f s] adds all formulas in the set [f] to the solver [s]. *)

val merge : t -> t -> t
(** [merge a b] adds all formulas in [b] to [a]. Both [a] and [b] must be "in" the global environment. *)
