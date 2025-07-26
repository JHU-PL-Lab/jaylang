
open Core

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

let to_arithmetic (type a b) (binop : (a * a * b) t) : a -> a -> b =
  match binop with
  | Plus -> ( + )
  | Minus -> ( - )
  | Times -> ( * )
  | Divide -> ( / )
  | Modulus -> ( mod )
  | Less_than -> ( < )
  | Less_than_eq -> ( <= )
  | Greater_than -> ( > )
  | Greater_than_eq -> ( >= )
  | Equal -> Poly.( = )
  | Not_equal -> Poly.( <> )
  | Or -> ( || )