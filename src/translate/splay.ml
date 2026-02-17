
type t = 
  | No
  | Yes_with_depth of int

let is_yes = function
  | No -> false
  | Yes_with_depth _ -> true
