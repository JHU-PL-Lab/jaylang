
module Make : functor (_ : Solve.S) (P : Pause.S) (_ : Options.V) -> sig
  val eval : Lang.Ast.Embedded.t -> Status.Terminal.t P.t
  (** [eval pgm] is the result of concolic evaluation on [pgm]. *)
end

val lwt_eval : (Lang.Ast.Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t
(** [lwt_eval pgm] is the reuslt of concolic evaluation on [pgm] using the default
    global [Solve.S] module. *)
