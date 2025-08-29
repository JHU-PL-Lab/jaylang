(**
  Module [Pack].

  Commonly in this repo when have a parametrized type ['a t], and we
  need to store [int t] and [bool t] together. This functor abstracts
  that pattern.

  We could alternatively have something like [type pack = Pack : 'a t -> pack],
  which means we are not limited to only [int] and [bool] parameters,
  but then we lose "type casing", and that generalization is not
  currently needed.
*)

module type S = sig
  type 'a x
  type t = 
    | I of int x
    | B of bool x
    [@@deriving compare]
  (** Pack [x] into an int [I] case and a bool [B] case. *)
end

module Make (X : Comparable.S1) : S with type 'a x := 'a X.t = struct
  open Core
  type t =
    | I of int X.t
    | B of bool X.t
    [@@deriving compare]
end
