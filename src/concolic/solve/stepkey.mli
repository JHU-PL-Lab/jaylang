(*
  File: stepkey.mli
  Purpose: uniquely represent a program point during interpretation

  Detailed description:
    Stepkeys represent a distinct point during interpretation under
    the assumption that the program execution path is known. That is,
    stepkeys can only be used to identify a program point along
    a specific execution path.

    This assumption is fine because with concolic execution, the entire
    execution path is constraint and solved for (that is, until interpretation
    reaches the branch that was solved for, and further branches taken
    are unconstrained). Stepkeys used during the solve only exist until
    the last constrained branch along the path.

    Therefore, stepkeys can be used to query the solver for inputs that
    force the execution down the desired path.
*)

include module type of Utils.Separate.Make_with_compare (Core.Int)
(** Is an identifier for a symbolic input at the interpreter step count
    given by the payload. *)

val to_string : 'a t -> string

val uniq_id : 'a t -> int
(** [uniq_id key] is the payload in [key]. *)
