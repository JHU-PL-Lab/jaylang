
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

module Packed : sig
  type t =
    | Dir_bool of bool T.t 
    | Dir_int of int T.t

  val compare : t -> t -> int
end

val pack : 'a t -> Packed.t
