(**
  Module [Identity].

  This is the "identity functor" but is actual identity.
*)

type 'a t = 'a [@@deriving compare]

let to_string (f : 'a -> string) (x : 'a t) : string = 
  f x
