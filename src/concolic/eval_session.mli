(**
  File: eval_session.mli
  Purpose: track one evaluation of a program

  Detailed description:
    The actual interpreter would be a mess if it had to handle
    the symbolic logic and options, so as much as possible is
    extracted into an "evaluation session". This builds up a
    stem, knows the concrete inputs needed to hit the target,
    and conveys the status after interpretation.

    That is, most symbolic information is stored here, and that
    way the interpreter only has to think about actually concretely
    interpreting the program, and it hands over some details of
    symbolic evaluation to this module.
*)

type t

(*
  --------
  CREATION
  --------
*)

val empty : t

val make : Target.t -> Input_feeder.t -> t

val with_options : (t, t) Options.Arrow.t

val get_max_step : t -> int

(*
  -----------
  EXPRESSIONS
  -----------
*)

val get_input : 'a Stepkey.t -> t -> t * Value.t
(** [get_input stepkey session] is the new session and a
    value that should be input for the given [stepkey]. *)

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : bool Direction.t -> bool Expression.t -> t -> t
(** [hit_branch dir expr session] hits the [dir], where the condition
    had the given [expr]. *)

val hit_case : int Direction.t -> int Expression.t -> other_cases:int list -> t -> t
(** [hit_branch dir expr other_cases session] hits the [dir], where the
    condition had the given [expr], and the [other_cases] of the int case
    are given in case the default case needs to be constructed. *)

val diverge : t -> Status.Eval.t
(** [diverge session] tells the [session] that the interpretation diverged. *)

val abort : t -> Status.Eval.t
(** [abort session] tells the [session] that the interpretation aborted. *)

val type_mismatch : t -> string -> Status.Eval.t
(** [type_mismatch session reason] tells the [session] that the interpretation ended
    due to type mismatch, explained by [reason]. *)

val reach_max_step : t -> Status.Eval.t
(** [reach_max_step session] tells the [session] that the interpretation ended
    due to reaching the max allowed step count. *)

val finish : t -> Status.Eval.t
(** [finish session] is the status of the [session], knowing that interpretation
    is now over. *)

