
include module type of Utils.Pack.Make (struct type 'a t = 'a [@@deriving compare] end)

val to_string : t -> string
