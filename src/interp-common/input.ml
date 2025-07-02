
(* can be packed into a list *)
include Utils.Pack.Make (Utils.Identity)

let to_string = function
  | I i -> Int.to_string i
  | B b -> Bool.to_string b
