
val lwt_eval : (Lang.Ast.Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t
(** [lwt_eval pgm] is the result of concolic evaluation on [pgm]. This does not use timeout. *)