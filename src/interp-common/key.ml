
type ('a, 'key) t = ('a, 'key) Utils.Separate.t

module Make (K : sig
  type t [@@deriving compare, equal]
  val to_string : t -> string
end) = struct
  type key = K.t
  include Utils.Separate.Make_with_compare (K)

  let to_string (type a) (x : a t) : string =
    match x with
    | I k -> Format.sprintf "ikey_$%s" (K.to_string k)
    | B k -> Format.sprintf "bkey_$%s" (K.to_string k)
end

module Stepkey = Make (Step)
module Indexkey = Make (Int)
module Stackkey = Make (Callstack)
module Timekey = Make (Timestamp)
