
type t [@@deriving compare]
val empty : t
val singleton : Z3.Expr.expr -> t
val add : t -> Z3.Expr.expr -> t
val add_multi : t -> Z3.Expr.expr list -> t
val union : t -> t -> t
val to_list : t -> Z3.Expr.expr list
val and_ : t -> Z3.Expr.expr
(* val or_ : t -> Z3.Expr.expr *) (* only used in V1 *)