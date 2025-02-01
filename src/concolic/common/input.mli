
include module type of Utils.Pack.Make (struct type 'a t = 'a [@@deriving compare] end)
