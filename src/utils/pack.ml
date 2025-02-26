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

open Core

module Make (X : sig type 'a t[@@deriving compare] end) = struct
  type t =
    | I of int X.t
    | B of bool X.t
    [@@deriving compare]
  (** Pack [X.t] into an int [I] case and a bool [B] case. *)
end