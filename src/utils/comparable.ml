
module type S = sig
  type t [@@deriving compare, equal]
end

module type S1 = sig
  type 'a t [@@deriving compare, equal]
end

module type P = sig
  include S
  val to_string : t -> string
end

module type P1 = sig
  include S1
  val to_string : ('a -> string) -> 'a t -> string
end
