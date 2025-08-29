
open Core

type 'key t = { get : 'a. ('a, 'key) Key.t -> 'a }[@@unboxed]

let zero : 'key t =
  let get (type a) (key : (a, 'key) Key.t) : a =
    match key with
    | I _ -> 0
    | B _ -> false
  in
  { get }

let default : 'key t =
  let get (type a) (key : (a, 'key) Key.t) : a =
    match key with
    | I _ -> Rand.int_incl (-10) 10
    | B _ -> Rand.bool ()
  in
  { get }

let of_smt_model ?(fallback_feeder : 'k t = default) (model : 'k Smt.Model.t) ~(uid : 'k -> int) : 'k t =
  let get (type a) (key : (a, 'k) Key.t) : a =
    let s : (a, 'k) Smt.Symbol.t = 
      match key with
      | I k -> Smt.Symbol.make_int k uid
      | B k -> Smt.Symbol.make_bool k uid
    in
    match model.value s with
    | Some v -> v
    | None -> fallback_feeder.get key
  in
  { get }

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

module Make (K : Utils.Comparable.P) = struct
  type nonrec t = K.t t

  let zero : t = zero
  let default : t = default

  module Key = Key.Make (K)
end
