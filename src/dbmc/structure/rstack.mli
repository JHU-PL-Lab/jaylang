include module type of Rstack_intf

val empty : t
val to_string : t -> string
val pp : Format.formatter -> t -> unit
val length : t -> int
val construct_stks : t -> frame list * frame list
val push : t -> frame -> t
val pop : t -> frame -> t option
val paired_callsite : t -> Id.t -> Id.t option
val pop_at_condtop : t -> frame -> bool * t
val relativize : Concrete_stack.t -> Concrete_stack.t -> t
val concretize_top : t -> Concrete_stack.t
