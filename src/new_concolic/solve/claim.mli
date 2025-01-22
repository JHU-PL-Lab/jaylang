
type 'a t = Equality : 'a Expression.t * 'a Direction.t -> 'a t

val to_formula : 'a t -> bool C_sudu.Gexpr.t

val direction : 'a t -> 'a Direction.t
