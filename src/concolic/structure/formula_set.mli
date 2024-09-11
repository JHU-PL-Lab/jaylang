
type t [@@deriving compare]
val empty : t
val singleton : Z3.Expr.expr -> t
val add : t -> Z3.Expr.expr -> t
val to_list : t -> Z3.Expr.expr list
val equal : t -> t -> bool