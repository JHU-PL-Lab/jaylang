
include Utils.Pack.Make (struct type 'a t = 'a [@@deriving compare] end)

let to_string = function
  | I i -> Int.to_string i
  | B b -> Bool.to_string b
