
open Core

module type KEY = sig
  type t
  val uid : t -> int
end

module X = Utils.Separate.Make_with_compare (Int)

module T = struct
  type ('a, 'k) t = 'a X.t

  let equal = X.equal

  let make_int (k : 'k) (uid : 'k -> int) : (int, 'k) t =
    I (uid k)

  let make_bool (k : 'k) (uid : 'k -> int) : (bool, 'k) t =
    B (uid k)
end

include T

module Make (Key : KEY) = struct
  type 'a t = ('a, Key.t) T.t

  let make_int (k : Key.t) : int t =
    T.make_int k Key.uid

  let make_bool (k : Key.t) : bool t =
    T.make_bool k Key.uid
end
