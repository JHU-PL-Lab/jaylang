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

module Monad : Types.MONAD with type 'a m = 'a = struct
  type 'a m = 'a
  let[@inline always] return a = a
  let[@inline always] bind x f = f x
end

module Transformer (M : Types.MONAD) : Types.TRANSFORMED with type 'a m = 'a M.m and type 'a lower := 'a M.m = struct
  include M 
  let upper m = m
  let map_t f = f
end
