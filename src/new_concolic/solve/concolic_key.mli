
type t [@@deriving compare, equal, sexp]

val create : int -> t
(** [create step_count] is the key for expression created at the interpreter [step_count]. *)

val to_string : t -> string

val uniq_id : t -> int
