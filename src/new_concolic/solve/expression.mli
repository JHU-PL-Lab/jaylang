
module Typed_binop : sig
  type _ t =
    | Plus : (int * int * int) t
    | Minus : (int * int * int) t
    | Times : (int * int * int) t
    | Divide : (int * int * int) t
    | Modulus : (int * int * int) t
    | Less_than : (int * int * bool) t
    | Less_than_eq : (int * int * bool) t
    | Greater_than : (int * int * bool) t
    | Greater_than_eq : (int * int * bool) t
    | Equal_int : (int * int * bool) t
    | Equal_bool : (bool * bool * bool) t
    | Not_equal : (int * int * bool) t
    | And : (bool * bool * bool) t
    | Or : (bool * bool * bool) t
end

type 'a t

val is_const : 'a t -> bool

val true_ : bool t

val const_int : int -> int t
val const_bool : bool -> bool t

val key : 'a Stepkey.t -> 'a t

val not_ : bool t -> bool t

val op : 'a t -> 'a t -> ('a * 'a * 'b) Typed_binop.t -> 'b t

val t_to_formula : 'a t -> 'a C_sudu.Gexpr.t
