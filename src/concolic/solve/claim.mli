
type 'a t = Equality of ('a Expression.t * 'a Direction.t) [@@unboxed]

val flip : bool t -> bool t

val direction : 'a t -> 'a Direction.t

val to_expression : 'a t -> bool Expression.t
