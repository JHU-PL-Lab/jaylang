
val eval
   : ?quit_on_first_abort:bool
  -> Jayil.Ast.expr
  -> Branch_tracker.Status_store.Without_payload.t
(** Tries to hit all branches in the expression and stops when there is nothing left.
    Prints the results and info during the run. 
    Returns the branch information after all the runs. *)

val eval_timeout : Jayil.Ast.expr -> float -> Branch_tracker.Status_store.Without_payload.t option
(** [eval_timeout expr s] is [Some (eval expr)] if the process terminated in fewer than [s] seconds. *)
