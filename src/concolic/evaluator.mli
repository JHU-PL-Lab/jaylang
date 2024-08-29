
val lwt_eval : (Jayil.Ast.expr, Session.Status.t Lwt.t) Options.Fun.t
(** [lwt_eval expr] is the branch info and whether the path tree was pruned. It quits on timeout based on the given options. *)