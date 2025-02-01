
include module type of Utils.Separate.Make_with_compare (Core.Int)
(** Is an identifier for a symbolic input at the interpreter step count
    given by the payload. *)

val to_string : 'a t -> string

val uniq_id : 'a t -> int
(** [uniq_id key] is the payload in [key]. *)
