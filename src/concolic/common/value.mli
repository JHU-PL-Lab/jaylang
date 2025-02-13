
include module type of Lang.Value.Embedded (struct type 'a t = 'a * 'a Expression.t let to_string f (v, _) = f v end)

include module type of T with type t = t
