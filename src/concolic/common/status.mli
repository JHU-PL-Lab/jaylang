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
*)

(* The following types are just constraints to use in the GADT. *)
type 'a terminal = 'a constraint 'a = [ `Terminal ]
type 'a eval = 'a constraint 'a = [ `Eval ]

type _ t =
  | Found_abort : Interp_common.Input.t list * string -> 'a t
  | Type_mismatch : Interp_common.Input.t list * string -> 'a t
  | Unbound_variable : Interp_common.Input.t list * Lang.Ast.Ident.t -> 'a t

  (* result from entire concolic evaluation *)
  | Exhausted_full_tree : 'a terminal t
  | Exhausted_pruned_tree : 'a terminal t
  | Unknown : 'a terminal t (* unknown due to solver timeout, but otherwise no error found, and a best attempt was made *)
  | Timeout : 'a terminal t

  (* result from a single run *)
  | Finished : { pruned : bool } -> 'a eval t

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

module Eval : sig type nonrec t = [ `Eval ] t end
module Terminal : sig type nonrec t = [ `Terminal ] t end
