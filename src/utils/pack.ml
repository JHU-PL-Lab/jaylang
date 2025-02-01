
open Core

module Make (X : sig type 'a t[@@deriving compare] end) = struct
  type t =
    | I of int X.t
    | B of bool X.t
    [@@deriving compare]
  (** Pack [X.t] into an int [I] case and a bool [B] case. *)
end