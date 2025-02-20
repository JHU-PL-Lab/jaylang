(**
  File: intra_session.mli
  Purpose: handle logic between each interpretation

  Detailed description:
    This session holds the path tree, and it accepts the result
    of evaluations (see [accum_eval]), adds them to the path
    tree, and then can tell whether to proceed with more evaluations
    of the program or to quit.

    The evaluator interfaces with this instead of the path tree. It is
    mainly just a layer on top of the path tree with a few extra
    details about the status of evaluation.
*)

module Make : functor (_ : Solve.S) (P : Pause.S) (_ : Options.V) -> sig
  type t
  (** [t] holds program info between interpretations and helps generate the evaluation session
      for the next run. *)

  val empty : t
  (** [empty] is a default empty session *)

  val accum_eval : t -> Status.Eval.t -> t
  (** [accum_eval t status] accumulates the resulting [status] into [t]. *)

  val next : t -> [ `Done of Status.Terminal.t | `Next of (t * Eval_session.t) ] P.t
  (** [next t] is [`Done status] if the concolic evaluation is done, or is [`Next (session, eval_session)]
  if the interpreter is to be run again with the [eval_session] session. *)

  val run_num : t -> int
  (** [run_num t] is the number of interpretations [t] has done. *)
end
