
include module type of Lang.Value.Embedded (struct type 'a t = 'a * 'a Expression.t end)

include module type of T with type t = t
