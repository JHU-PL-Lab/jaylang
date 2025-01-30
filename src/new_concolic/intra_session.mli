
type t
(** [t] holds program info between interpretations and helps generate the evaluation session
    for the next run. *)

(* val empty : t *)
(** [empty] is a default empty session *)

val of_options : (unit, t * Eval_session.t) Options.Fun.a
(** [of_options ()] is an empty session that has all relevant info loaded in from the optional arguments,
    along with an eval session for the first interpreter run. *)

val accum_eval : t -> Eval_session.t -> t
(** [accum_eval t sess] finishes the eval session [sess] and accumulates results into [t]. *)

val next : t -> [ `Done of Status.Terminal.t | `Next of (t * Eval_session.t) ] Lwt.t
(** [next t] is [`Done status] if the concolic evaluation is done, or is [`Next (session, eval_session)]
   if the interpreter is to be run again with the [eval_session] session. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)