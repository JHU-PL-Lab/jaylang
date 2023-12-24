(*
  File: branch_solver.mli
  Purpose: help store information about runtime variables and solve to hit branches.

  Detailed description:
    This module defines representations of runtime data to solve to hit branches
    in an AST. Values of runtime variables can be added to the solver underneath
    the parent branch as found during the interpretation of the program. Then a
    branch can be picked and solved for, and an input feeder is returned to attempt
    to hit that branch.

  Logic description:

    TODO: update this description

    In this section I attempt to describe *how* the solver will work and will be used
    by the concolic evaluator.

    Suppose the concolic evaluator recursively evaluates the nodes in the AST like an
    interpreter, and there is a parent branch (i.e. a condition and the direction of an
    "if" statement--or no parent, which is the global case and trivial) above the current
    node.
    
    We maintain a parent store that stores the parents for each variable (i.e. the direction
    that a condition *must* take in order for that variable to take on the value it just did).
    We can call these parents the "dependencies" because the variable depends on the parents
    taking on those directions.
    * Note: any variable we are considering will NOT have the current parent as a parent in
      the parent store. The variables will only depend on parents that are deeper in the tree
      because they are within some inner branch or depend on some variable that is within some
      inner branch.

    We also maintain a formula store that the parents imply (using an actual "implies" formula
    eventually). This formula store will hold under each parent all the variables and their values,
    and upon exiting that parent branch, we accumulate all of the formulas and create an "implies"
    statement that the parent implies those formulas, and we put it under the parent of the parent.
    e.g.
      Outer parent
        <some code>
        Inner parent
          /* suppose WLOG we take the "true" direction */
          x = 0 /* the inner parent has a formula under it that x = 0 */
          y = 1 /* the inner parent has a formula under it that y = 1 */
        /* exiting inner parent branch... */
        /* the outer parent has a formula that (inner parent = true) => (x = 0 and y = 1) */
        z = result of inner parent /* outer parent has formula z = y */

    To save space, we clear out all the formulas under the branch after exiting because they won't
    be needed again, and anything that *is* needed will be necessarily stored under the outer parent
    under a big "anded" expression.

    The only time a parent is directly added to a variable is upon exiting a branch. In the example
    above, z takes on the result of the inner branch, and it directly gains the parent
      ( inner branch , true ).
    Now, whenever any later variable depends on z (e.g. if we add w = z to the next line), that variable
    gains any parents that z had because now it also depends on z's parents. This is how parents originate
    and are accumulated. Note that this means no variable EVER has the outer parent as a dependency
    while underneath the outer parent branch.

    In this formula store, under the global scope, are many "pick" formulas, where each branch key
    (i.e. the variable that identifies the branch clause) implies all the parents above that
    are necessary to reach it. Then, upon solving, we "set one of these branch keys to true" (very
    roughly) so that the parents must all be satisfied by the solver.

    TODO: since we're only ever adding to the parent we're under (or the global parent, in which case
      I might just want to add to the solver), we can pass along a list of expressions for the current
      parent and keep on the stack frame the list for upper parents. Equivalently, we could have a 2D
      list, where the head is the expression list for the current parent, and when done with that parent
      we just pop off the head and return to the tail.
      I can assert that the user is doing this properly by making it a list of (branch * expr list) so that
      I check the branch is indeed the parent we're under.
*)

exception NoParentException
(** Used to convey that the user of the solver is trying to add a formula or back up to
    a previous parent when there is no such parent. If the solver is used properly on a
    valid Jayil program, this should be logically impossible. *)

module Formula_set :
  sig
    type t
    val add : t -> Z3.Expr.expr -> t
    val union : t -> t -> t
    val fold : t -> init:'a -> f:('a -> Z3.Expr.expr -> 'a) -> 'a
    val empty : t
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

val add_formula : t -> Z3.Expr.expr -> t (* TODO: hide *)
(** [add_formula s e] adds the z3 expression [e] to the solver [s]. *)

val get_feeder : t -> Branch.Runtime.t -> (Concolic_feeder.t, Branch.Ast_branch.t) result
(** [get_feeder s b] is a feeder to hit the branch [b] based on formulas in the solver [s], 
    or is an error if [b] is unsatisfiable. *)

val get_cur_parent_exn : t -> Branch.Runtime.t
(** [get_cur_parent_exn s] is the branch that defines current environment of the solver [s],
    or is an exception if [s] is in the global environment. *)

val add_formula_set : Formula_set.t -> t -> t
(** [add_formula_set f s] adds all formulas in the set [f] to the solver [s]. *)

val merge : t -> t -> t
(** [merge a b] adds all formulas in [b] to [a]. Both [a] and [b] must be "in" the global environment. *)
