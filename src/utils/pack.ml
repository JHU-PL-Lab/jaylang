
open Core

module Make (P : sig type 'a t[@@deriving compare] end) = struct
  type t =
    | I of int P.t
    | B of bool P.t
    [@@deriving compare]
end