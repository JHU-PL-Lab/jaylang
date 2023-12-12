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


module Runtime_branch :
  sig
    type t =
      { condition_key : Lookup_key.t
      ; direction : Ast_branch.Direction.t }
    (** [t] represents a branch direction during runtime, where the condition key has been
        evaluated to some true/false direction. The condition is a [Lookup_key.t] because it
        considers the current stack and is not just an identifier in the AST.

        Note: we represent an AST branch by its branch key because that is unique to
          the AST node (the variable that is assigned in the clause), but the runtime
          branch is represented by the condition because the condition actually takes on
          a true or false value. This allows us to actually solve for a direction in a
          runtime branch. *)

    val other_direction : t -> t
  end

module Target :
  sig
    type t =
      { branch_key : Lookup_key.t
      ; branch     : Runtime_branch.t }
    (** [t] is a branch in the AST with its condition and direction such that the branch can
        be solved for. the [branch_key] is the key of the clause to identify the node in the
        AST, and the [branch] is the condition key and its direction in order to solve for the
        condition variable to attain the desired true/false value. *)

    val to_string : t -> string
    val print_option : t option -> unit

    val to_branch : t -> Ast_branch.t
    (** [to_branch x] is an AST branch, where it is the branch ident (the clause
        ident) and the direction the condition [x.branch.condition_key] will
        evaluate to. The condition key is discarded. *)

    val other_direction : t -> t
    (** [other_direction x] is a target branch where the runtime branch [x.branch] is the other
        direction. e.g. if [x.branch] is [false], then [(other_direction x).branch] is [true].*)
  end

module Parent :
  sig
    type t =
      | Global
      | Local of Runtime_branch.t
    (** [t] represents some parent environment to a clause. A clause either is underneath
        some branch (e.g. the "false" direction of an if-statement), or is under no branch
        and is therefore in the global environment.
        
        A parent is a runtime branch instead of an AST branch so that the evaluation of the
        parent to some direction can imply the clause. The AST branch only considers the
        ident of the branch clause (not the condition), so it has no true/false value. *)

    val of_runtime_branch : Runtime_branch.t -> t
    (** [of_runtime_branch x] is [x] wrapped in "Local". *)

    val to_ast_branch_exn : t -> Ast_branch.t
    (** [to_ast_branch_exn x] is the AST branch representation of [x], or an exception if
        [x] is the global parent. *)
  end

type t
(** [t] will hold all the formulas for a solver to eventually use. It contains information
    about the parents of each clause, what a clause depends on, and what formulas the clause
    implies. 
    
    I sometimes call this a "store" because it stores the information. *)

val empty : t
(** [empty] is a branch solver with no information at all. *)

val generate_lookup_key : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> Lookup_key.t
(** [generate_lookup_key x stk] is a lookup key for the variable [x] where the key considers the 
    current stack [stk], so the variable is identifiable as a runtime variable, e.g. the same variable
    name in different calls of a recursive function have different lookup keys. *)

val add_formula : Lookup_key.t list -> Parent.t -> Z3.Expr.expr -> t -> t
(** [add_formula deps parent formula store] is a new store where the [formula] is added underneath
    the [parent]. The formula depends on all the keys [deps], so the parents (that imply all the [deps]),
    all imply the [formula]. *)

val add_key_eq_value_opt : Parent.t -> Lookup_key.t -> Jayil.Ast.value option -> t -> t
(** [add_key_eq_value_opt parent key val_opt store] is a new store that contains a formula where the [key]
    equals the value of [val_opt], or the [key] equals itself if [val_opt] is [None]. The formula is added
    under the [parent] in the [store]. *)

val add_siblings : Lookup_key.t -> Lookup_key.t list -> t -> t
(** [add_siblings child_key siblings store] is a new store where the [child_key] now acquires all the
    same parents as the keys in [siblings], and these parents are strictly in addition to the parents
    that the [child_key] already has. This is used to make [child_key] depend on everything that all the
    [siblings] depend on.
    Only information about [child_key] is updated in the store, and nothing is changed for the [siblings]. *)

(* Note: no longer needed -- is included in exit_branch *)
(* val add_pick_branch : Lookup_key.t -> Parent.t -> t -> t *)
(** [add_pick_branch branch_key parent store] is a new store with an added formula in the global scope such
    that [branch_key] can be solved for by picking the [branch_key] later.
    
    This is done by letting all parents of [branch_key], starting with [parent], be formulas that are
    implied by [branch_key], so that picking [branch_key] later means all parents must be satisfied by
    in the solve. *)

val exit_branch : Lookup_key.t -> Parent.t -> Runtime_branch.t -> Lookup_key.t -> t -> t
(** [exit_branch branch_key parent exited_branch result_key store] leaves the branch [exited_branch] that
    is the clause body for [branch_key]. 
    The [parent] is the parent of the [branch_key]. The [result_key] is the key for the last evaluated
    clause in the branch, and it gets assigned to [branch_key]. 
    
    Four things happen in this function:
    1. Accumulates all the formulas under the branch that was just evaluated and assigns
      them under the [parent] (as implied by [exited_branch.condition_key] equals [exited_branch.direction]).
    2. Sets [branch_key] to be a child of [exited_branch] as a parent
    3. Sets [branch_key] to be a sibling of [result_key].
    4. Clears out all formulas under [condition_key] because the branch cannot be entered again,
      and any needed formulas are now encompassed by the first step.
      TODO: consider not clearing branches.
    5. Adds a pick formula to the global scope so that the branch can be picked and solved for later, if wanted *)

val get_feeder : Target.t -> t -> (Concolic_feeder.t, Ast_branch.t) result
(** [get_feeder target store] uses the formulas in the [store] under the global
    scope and uses a Z3 solver to solve for the [target] branch.
    If the formulas in the [store] are satisfiable, then a feeder is returned
    for the next concolic run to try to hit the target branch. *)