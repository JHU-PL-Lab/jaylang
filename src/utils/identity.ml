(**
  Module [Identity].

  This is the "identity functor" but is actual identity.
*)

type 'a t = 'a [@@deriving compare]

let to_string (f : 'a -> string) (x : 'a t) : string = 
  f x

let put a = a

let get a = a

let equal eq a b = eq a b
