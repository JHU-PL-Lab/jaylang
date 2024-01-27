
val eval
   : ?timeout_sec:float
  -> ?quit_on_first_abort:bool
  -> Jayil.Ast.expr
  -> Branch_tracker.Status_store.Without_payload.t
(** Tries to hit all branches in the expression and stops when there is nothing left.
    Prints the results and info during the run. 
    Returns the branch information after all the runs. The result is empty if it times out. *)