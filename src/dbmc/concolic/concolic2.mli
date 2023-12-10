
val concolic_eval : Jayil.Ast.expr -> unit
(** Tries to hit all branches beginning with a random input and stopping
    when all have been hit, the max steps have been exceeded, or a branch
    is unsatisfiable. *)