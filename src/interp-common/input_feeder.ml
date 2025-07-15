
open Core

type 'key t = { get : 'a. ('a, 'key) Key.t -> 'a }[@@unboxed]

let zero : 'key t =
  { get = 
    let f (type a) (key : (a, 'key) Key.t) : a =
      match key with
      | I _ -> 0
      | B _ -> false
    in
    f
  }

let default : 'key t =
  { get = 
    let f (type a) (key : (a, 'key) Key.t) : a =
      match key with
      | I _ -> Rand.int_incl (-10) 10
      | B _ -> Rand.bool ()
    in
    f
  }

(*
  Feeds using index in the sequence
*)
let of_sequence (ls : Input.t list) : int t =
  let mt = Int.Map.empty in
  let m_ints, m_bools, _ =
    List.fold ls ~init:(mt, mt, 0) ~f:(fun (mi, mb, n) input ->
      match input with
      | Input.I i -> (Map.set mi ~key:n ~data:i, mb, n + 1)
      | Input.B b -> (mi, Map.set mb ~key:n ~data:b, n + 1)
    )
  in
  let get : type a. (a, int) Key.t -> a = fun key ->
    let a_opt : a option =
      match key with
      | I k -> Map.find m_ints k
      | B k -> Map.find m_bools k
    in
    Option.value a_opt ~default:(zero.get key)
  in
  { get }

module Make (K : sig
  type t [@@deriving compare, equal]
  val to_string : t -> string
end) = struct
  type nonrec t = K.t t

  let zero : t = zero
  let default : t = default

  module Key = Key.Make (K)
end
