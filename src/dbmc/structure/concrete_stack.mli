(** Use an interface to encapsulate the concrete stack *)

type t [@@deriving sexp, compare, equal, show { with_path = false }, hash]

val empty : t

val to_string : t -> string

val of_string : string -> t

val to_list : t -> (Id.t * Id.t) list

val of_list : (Id.t * Id.t) list -> t

val push : Id.t * Id.t -> t -> t

val equal_flip : t -> t -> bool
