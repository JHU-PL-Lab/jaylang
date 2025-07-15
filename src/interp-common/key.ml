
(* Some key to identify a program point. *)
module type S = sig
  type key
  include Utils.Separate.S with type x := key

  val to_string : 'a t -> string

  val equal : 'a t -> 'a t -> bool
  val compare : 'a t -> 'a t -> int
end

module Make (C : sig
  type t [@@deriving compare, equal]
  val to_string : t -> string
end) : S with type key = C.t = struct
  type key = C.t
  include Utils.Separate.Make_with_compare (C)

  let to_string (type a) (x : a t) : string =
    match x with
    | I c -> Format.sprintf "ikey_$%s" (C.to_string c)
    | B c -> Format.sprintf "bkey_$%s" (C.to_string c)
end

module Stepkey = Make (Step) (* steps until input*)
module Indexkey = Make (Int) (* index of input in sequence *)
module Stackkey = Make (Callstack) (* callstack as of input *)
module Timekey = Make (Timestamp) (* time (as invented for deferred semantics) at input *)
