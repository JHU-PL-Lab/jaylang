
module Binop : sig
  type iii = int * int * int
  type iib = int * int * bool
  type bbb = bool * bool * bool

  type _ t =
    | Plus : iii t
    | Minus : iii t
    | Times : iii t
    | Divide : iii t
    | Modulus : iii t
    | Less_than : iib t
    | Less_than_eq : iib t
    | Greater_than : iib t
    | Greater_than_eq : iib t
    | Equal : ('a * 'a * bool) t
    | Not_equal : ('a * 'a * bool) t
    | Or : bbb t
end

module type KEY = sig
  type t
  val uid : t -> int
end

module Symbol : sig 
  type ('a, 'k) t

  val equal : ('a, 'k) t -> ('a, 'k) t -> bool

  val make_int : 'k -> ('k -> int) -> (int, 'k) t

  val make_bool : 'k -> ('k -> int) -> (bool, 'k) t
end

module Make_symbol (Key : KEY) : sig
  type 'a t = ('a, Key.t) Symbol.t

  val make_int : Key.t -> int t

  val make_bool : Key.t -> bool t
end

module type S = sig
  type ('a, 'k) t

  val equal : ('a, 'k) t -> ('a, 'k) t -> bool

  val const_int : int -> (int, 'k) t

  val const_bool : bool -> (bool, 'k) t

  val symbol : ('a, 'k) Symbol.t -> ('a, 'k) t

  val not_ : (bool, 'k) t -> (bool, 'k) t

  val binop : ('a * 'a * 'b) Binop.t -> ('a, 'k) t -> ('a, 'k) t -> ('b, 'k) t

  val is_const : ('a, 'k) t -> bool

  val and_ : (bool, 'k) t list -> (bool, 'k) t
end

module Model : sig
  type 'k t = { value : 'a. ('a, 'k) Symbol.t -> 'a option }

  val empty : 'k t
end

type 'k model = 'k Model.t

type 'k solution =
  | Sat of 'k model 
  | Unknown
  | Unsat

module type SOLVABLE = sig
  include S

  val solve : (bool, 'k) t list -> 'k solution
end

module Make_Z3 () : SOLVABLE

module T : S

include S with type ('a, 'k) t = ('a, 'k) T.t

module Transform (X : S) : sig
  val transform : ('a, 'k) t -> ('a, 'k) X.t
end

(*
  Solves by transforming to the solvable expressions.
*)
module Solve (_ : SOLVABLE) : sig
  val solve : (bool, 'k) t list -> 'k solution
end
