
type _ t =
  | Int_key : int -> int t
  | Bool_key : int -> bool t
  (** Is an identifier for a symbolic input at the interpreter step count
      given by the payload. *)

val compare : 'a t -> 'a t -> int

val equal : 'a t -> 'a t -> bool

val to_string : 'a t -> string

val uniq_id : 'a t -> int
(** [uniq_id key] is the payload in [key]. *)
