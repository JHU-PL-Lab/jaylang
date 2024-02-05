
type t

val empty : t
(** [empty] is a default path tracker with no target and empty tree and stack. *)

val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in the top node of [t]. *)

val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in the top node of [t]. *)

val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] to the the top node of [t]. *)

val add_input : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_input t x v] is [t] that knows input [x = v] was given. *)

val finish : t -> t
(** [finish t] finishes [t] after a run through the interpreter. *)