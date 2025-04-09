
module Concolic_value = struct
  type 'a t = 'a * 'a Expression.t
  let to_string f (v, _) = f v
end

include Lang.Value.Embedded (Concolic_value)

include T
