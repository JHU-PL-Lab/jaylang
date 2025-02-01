
module T : sig
  type _ t =
    | True_direction : bool t
    | False_direction : bool t
    | Case_int : int -> int t
    | Case_default : { not_in : int list; } -> int t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
end

include module type of T with type 'a t = 'a T.t

val of_bool : bool -> bool t
val of_int : int -> int t

module Packed : module type of Utils.Pack.Make (T)

val pack : 'a t -> Packed.t
