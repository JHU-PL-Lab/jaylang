
open Core

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

module Using_indexkey = struct
  module Indexkey = Key.Indexkey
  include Make (Indexkey)

  let of_sequence (ls : Input.t list) : t =
    let mt = Int.Map.empty in
    let m_ints, m_bools, _ =
      List.fold ls ~init:(mt, mt, 0) ~f:(fun (mi, mb, n) input ->
        match input with
        | Input.I i -> (Map.set mi ~key:n ~data:i, mb, n + 1)
        | Input.B b -> (mi, Map.set mb ~key:n ~data:b, n + 1)
      )
    in
    let get : type a. a Key.Indexkey.t -> a = fun key ->
      let a_opt : a option =
        match key with
        | I k -> Map.find m_ints k
        | B k -> Map.find m_bools k
      in
      Option.value a_opt ~default:(zero.get key)
    in
    { get }
end

module Using_stackkey = struct
  module Stackkey = Key.Stackkey
  include Make (Stackkey)
end

module Using_timekey = struct
  module Timekey = Key.Timekey
  include Make (Timekey)
end