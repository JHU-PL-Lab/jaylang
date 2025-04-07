(**
  File: status.mli
  Purpose: convey the status of concolic evaluator components

  Detailed description:
    The concolic evaluator needs to frequently check what happened
    as a result of a process, or if a process is done, etc.

    This module puts many of those statuses in one place to be reused.

    For example, an abort may be hit during interpretation, which then
    propagates up the layers until it is shared as the final status
    of the entire concolic algorithm.

    GADTs and polymorphic variants are used for subtyping and reuse.

  Dependencies:
    Input -- in order to convey how to hit an abort of type mismatch
    Stem -- part of the result from interpretation
*)

(* The following three types are just constraints to use in the GADT. *)
type 'a terminal = 'a constraint 'a = [ `Terminal ]
type 'a eval = 'a constraint 'a = [ `Eval ]
type 'a in_progress = 'a constraint 'a = [ `In_progress ]

type _ t =
  | Found_abort : Input.t list * string -> 'a t
  | Type_mismatch : Input.t list * string -> 'a t
  | Unbound_variable : Input.t list * Lang.Ast.Ident.t -> 'a t

  (* result from entire concolic evaluation *)
  | Exhausted_full_tree : 'a terminal t
  | Exhausted_pruned_tree : 'a terminal t
  | Timeout : 'a terminal t

  (* result from a single run *)
  | Finished : { pruned : bool ; reached_max_step : bool ; stem : Stem.t } -> 'a eval t

  (* status while evaluation is ongoing *)
  | Diverge : 'a in_progress t
  | In_progress : 'a in_progress t

val is_terminal : 'a t -> bool
(** [is_terminal status] is true if and only if the [status] indicates the end
    of all concolic evaluation (e.g. that the entire path tree was exhausted). *)

val is_error_found : 'a t -> bool
(** [is_error_found status] is true if and only if the [status] indicates that
    an error was positively found in the target program. *)

val to_string : 'a t -> string
(** [to_string status] is a nice string representation of the [status]. *)

val to_loud_string : 'a t -> string
(** [to_loud_string status] is the string representation of [status], but where the meat
    of the message is emphasized with capitalization. *)

module In_progress : sig type nonrec t = [ `In_progress ] t end
module Eval : sig type nonrec t = [ `Eval ] t end
module Terminal : sig type nonrec t = [ `Terminal ] t end
