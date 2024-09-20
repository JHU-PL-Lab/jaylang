
module Symbolic = Symbolic_session
(** [Symbolic] is alias for [Symbolic_session]. *)

module Status :
  sig
    type t =
      | In_progress of { pruned : bool }
      | Found_abort of (Branch.t * Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jil_input.t list [@compare.ignore])
      | Exhausted of { pruned : bool }
      [@@deriving compare, sexp]

    val to_string : t -> string
  end

type t
(** [t] holds program info between interpretations and helps generate the symbolic session
    for the next run. *)

(* val empty : t *)
(** [empty] is a default empty session *)

val of_options : (unit, t * Symbolic.t) Options.Fun.p
(** [of_options ()] is an empty session that has all relevant info loaded in from the optional arguments,
    along with a symbolic session for the first interpreter run. *)

val accum_symbolic : t -> Symbolic.t -> t
(** [accum_symbolic t sym] finishes the sybolic session [sym] and accumulates results into [t]. *)

val next : t -> [ `Done of Status.t | `Next of (t * Symbolic.t) ] Lwt.t
(** [next t] is [`Done status] if the concolic evaluation is done, or is [`Next (session, symbolic)]
   if the interpreter is to be run again with the [symbolic] session. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)