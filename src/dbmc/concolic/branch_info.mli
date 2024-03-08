
module Status :
  sig
    type t = 
      | Found_abort
      | Hit
      | Unknown
      | Unhit
      [@@deriving compare, sexp]

    val to_string : t -> string
  end

type t [@@deriving sexp]

val empty : t

val of_expr : Jayil.Ast.expr -> t

val to_string : t -> string

val set_branch_status : new_status:Status.t -> t-> Branch.t -> t

val contains : t -> Status.t -> bool

val merge : t -> t -> t

val find : t -> f:(Branch.t -> Status.t -> bool) -> (Branch.t * Status.t) option