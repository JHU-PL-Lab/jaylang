
module type S = sig
  type t [@@deriving equal]
end

module type S1 = sig
  type 'a t [@@deriving equal]
end

(*
  P1 for "printable with 1 type parameter".
  And is in the Equatable module, so it comes
  with equality too.
*)
module type P1 = sig
  include S1
  val to_string : ('a -> string) -> 'a t -> string
end