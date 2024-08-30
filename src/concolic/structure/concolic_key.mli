
type t [@@deriving compare, equal, sexp]

val create : Jayil.Ast.Ident_new.t -> int -> t
(** [create id step_count] is the key for the clause [id] created at the interpreter [step_count] *)

val to_string : t -> string

val x : t -> int

val id : t -> Jayil.Ast.Ident_new.t
