
module Make_type (K : Key.S) = struct
  type t = { get : 'a. 'a K.t -> 'a } [@@unboxed]

  (* 
    Cannot have a general, non-functorized 'k t because OCaml
    does not have higher order type parameters.
  *)
end

module Make_zero (K : Key.S) = struct
  include Make_type (K)

  let zero : t =
    { get = 
      let f (type a) (key : a K.t) : a =
        match key with
        | I _ -> 0
        | B _ -> false
      in
      f
    }
end

module Make_default (K : Key.S) = struct
  include Make_type (K)

  let default : t =
    { get = 
      let f (type a) (key : a K.t) : a =
        match key with
        | I _ -> Rand.int_incl (-10) 10
        | B _ -> Rand.bool ()
      in
      f
    }
end

module Make (K : Key.S) = struct
  include Make_zero (K)
  include Make_default (K)
end

module Using_stepkey = struct 
  module Stepkey = Key.Stepkey
  include Make (Stepkey)
end

module Using_stackkey = struct
  module Stackkey = Key.Stackkey
  include Make (Stackkey)
end