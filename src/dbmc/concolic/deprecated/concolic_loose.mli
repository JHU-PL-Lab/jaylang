
val eval : (Jayil.Ast.expr -> Branch_tracker.Status_store.Without_payload.t) Concolic_options.F.t
(** Tries to hit all branches in the expression and stops when there is nothing left.
    Prints the results and info during the run. 
    Returns the branch information after all the runs. The result is empty if it times out.
    Is "loose" because it allows the solver to take any path to reach any instance of a branch. *)