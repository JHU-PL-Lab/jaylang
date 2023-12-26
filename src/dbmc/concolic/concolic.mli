
val concolic_eval : Jayil.Ast.expr -> unit
(** Tries to hit all branches in the expression and stops when there is nothing left.
    Prints the results and info during the run. *)