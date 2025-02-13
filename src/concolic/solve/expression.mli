
module Typed_binop : sig
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
    | Equal_int : iib t
    | Equal_bool : bbb t
    | Not_equal : iib t
    | And : bbb t
    | Or : bbb t
end

type 'a t

val is_const : 'a t -> bool

val true_ : bool t
val false_ : bool t

val const_int : int -> int t
val const_bool : bool -> bool t

val key : 'a Stepkey.t -> 'a t

val not_ : bool t -> bool t

val op : 'a t -> 'a t -> ('a * 'a * 'b) Typed_binop.t -> 'b t

module Solve (Expr : Z3_intf.S) : sig
  val t_to_formula : 'a t -> 'a Expr.t
end
