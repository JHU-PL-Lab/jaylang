
val eval : Jayil.Ast.expr -> Branch_tracker.Status_store.t
(** Tries to hit all branches in the expression and stops when there is nothing left.
    Prints the results and info during the run. 
    Returns the branch information after all the runs. *)