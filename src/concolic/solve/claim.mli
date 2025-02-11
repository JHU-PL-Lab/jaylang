
type 'a t = Equality : 'a Expression.t * 'a Direction.t -> 'a t

val flip : bool t -> bool t

val direction : 'a t -> 'a Direction.t

val to_expression : 'a t -> bool Expression.t

(* module Make (Expr : module type of Expression) : sig
  val to_formula : 'a t -> bool .t
end *)

