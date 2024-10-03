
type t [@@deriving compare, equal, sexp]

val create : Jayil.Ast.Ident_new.t -> int -> bool -> t
(** [create id step_count is_const] is the key for the clause [id] created at the interpreter [step_count]
    where the value of the clause is constant (i.e. not dependent on inputs) if and only if [is_const]. *)

val with_dependencies : t -> t list -> t
(** [with_dependencies t deps] sets the [is_const] label in [t] to false if and only if some key in [deps]
    is not const. Otherwise, [t] is untouched. *)

val set_not_const : t -> t

val to_string : t -> string

val x : t -> int

val id : t -> Jayil.Ast.Ident_new.t

val is_const : t -> bool
