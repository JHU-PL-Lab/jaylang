type frame = Id.t * Id.t [@@deriving hash, equal]

type t [@@deriving sexp_of, compare, equal, hash]

val empty : t

val to_string : t -> string

val hash : t -> int
(* val show : t -> string *)

val pp : Format.formatter -> t -> unit

val equal_frame : frame -> frame -> bool

val construct_stks : t -> frame list * frame list

val relativize : Concrete_stack.t -> Concrete_stack.t -> t

val push : t -> frame -> t

val pop : t -> frame -> t option

val concretize_top : t -> Concrete_stack.t

val paired_callsite : t -> Id.t -> Id.t option
