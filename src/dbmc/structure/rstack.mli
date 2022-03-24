type frame = Id.t * Id.t [@@deriving hash, equal]
type op = Push | Co_pop [@@deriving hash, equal]
type t [@@deriving hash, equal, compare, sexp_of]

val empty : t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val length : t -> int
val construct_stks : t -> frame list * frame list
val push : t -> frame -> t
val pop : t -> frame -> t option
val relativize : Concrete_stack.t -> Concrete_stack.t -> t
val concretize_top : t -> Concrete_stack.t
val paired_callsite : t -> Id.t -> Id.t option
