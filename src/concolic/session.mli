
(**
  The concrete session tracks everything needed by the interpreter to evaluate the expression.
  Nothing here is used for any part of the concolic logic. This is all to correctly evaluate
  the expression.
*)
module Concrete :
  sig
    (* NOTE: this type is mutable *)
    type t =
      { input_feeder    : Concolic_feeder.t
      ; mutable step    : int
      ; max_step        : int }

    val create_default : unit -> t
    (** [create_default ()] is an arbitrary session with no intentional input feeder and empty graphs. *)

    val create : Concolic_feeder.t -> int -> t
    (** [create input_feeder global_max_step] *)

    val incr_step : t -> unit
    (** [incr_step t] has the side effect of incrementing the [step] field of the mutable session [t]. *)
  end

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
(** [t] holds program info between interpretations and helps generate concrete and symbolic sessions
    for the next run. *)

val empty : t
(** [empty] is a default empty session *)

val with_options : (t, t) Options.Fun.t
(** [with_options t] is [t] that has all relevant info loaded in from the optional arguments. *)

val accum_symbolic : t -> Symbolic.t -> t
(** [accum_symbolic t sym] finishes the sybolic session [sym] and accumulates results into [t]. *)

val next : t -> [ `Done of Status.t | `Next of (t * Symbolic.t * Concrete.t) ] Lwt.t
(** [next t] is [`Done status] if the concolic evaluation is done, or is [`Next (session, symbolic, concrete)]
   if the interpreter is to be run again with [symbolic] and [concrete] sessions. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)