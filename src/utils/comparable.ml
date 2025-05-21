
module type S = sig
  type t [@@deriving compare]
end

module type S1 = sig
  type 'a t [@@deriving compare]
end