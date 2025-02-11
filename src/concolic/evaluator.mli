
module New_context () : sig
  module Make (P : Pause.S) (_ : Options.V) : sig
    val eval : Lang.Ast.Embedded.t -> Status.Terminal.t P.t
    (** [eval pgm] is the result of concolic evaluation on [pgm]. *)
  end
  
end

val lwt_eval : (Lang.Ast.Embedded.t, Status.Terminal.t Lwt.t) Options.Arrow.t
