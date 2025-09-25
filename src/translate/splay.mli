
type t = 
  | No
  | Yes_with_depth of int

val is_yes : t -> bool
