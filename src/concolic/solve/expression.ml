
open Core

module Const = struct
  type _ t =
    | I : int -> int t
    | B : bool -> bool t

  let equal (type a) (x : a t) (y : a t) : bool =
    match x, y with
    | I i1, I i2 -> i1 = i2
    | B b1, B b2 -> Bool.(b1 = b2)

  let box_int i = I i
  let box_bool b = B b
end

open Const

module Typed_binop = struct
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

  let equal (type a) (x : a t) (y : a t) : bool =
    match x, y with
    | Plus, Plus
    | Minus, Minus
    | Times, Times
    | Divide, Divide
    | Modulus, Modulus
    | Less_than, Less_than
    | Less_than_eq, Less_than_eq
    | Greater_than, Greater_than
    | Greater_than_eq, Greater_than_eq
    | Equal_int, Equal_int
    | Equal_bool, Equal_bool
    | Not_equal, Not_equal
    | And, And
    | Or, Or -> true
    | _ -> false

  let to_arithmetic (type a b) (binop : (a * a * b) t) : a Const.t -> a Const.t -> b Const.t =
    let op
      : type a b. (a -> a -> b) -> (b -> b Const.t) -> a Const.t -> a Const.t -> b Const.t
      = fun op ret x y ->
        ret @@
          match x, y with
          | (I a), (I b) -> op a b
          | (B a), (B b) -> op a b
    in
    match binop with
    | Plus -> op ( + ) box_int
    | Minus -> op ( - ) box_int
    | Times -> op ( * ) box_int
    | Divide -> op ( / ) box_int
    | Modulus -> op ( mod ) box_int
    | Less_than -> op ( < ) box_bool
    | Less_than_eq -> op ( <= ) box_bool
    | Greater_than -> op ( > ) box_bool
    | Greater_than_eq -> op ( >= ) box_bool
    | Equal_int -> op ( = ) box_bool
    | Equal_bool -> op Bool.( = ) box_bool
    | Not_equal -> op ( <> ) box_bool
    | And -> op ( && ) box_bool
    | Or -> op ( || ) box_bool
end

type _ t =
  | Const : 'a Const.t -> 'a t
  (* The following constructors form abstract expressions *)
  | Key : 'a Stepkey.t -> 'a t
  | Not : bool t -> bool t
  | Binop : ('a * 'a * 'b) Typed_binop.t * 'a t * 'a t -> 'b t

let rec equal : type a. a t -> a t -> bool =
  fun x y ->
    match x, y with
    | Const c1, Const c2 -> Const.equal c1 c2
    | Key k1, Key k2 ->
      Stepkey.equal k1 k2
    | Not e1, Not e2 -> equal e1 e2
    (* Type checking is hard here. Just don't compare binops for now. Say they are always different *)
    (* | Binop (op1, left1, right1), Binop (op2, left2, right2) ->
      Typed_binop.equal op1 op2
      && equal left1 left2
      && equal right1 right2 *)
    | _ -> false

let is_const : type a. a t -> bool = function
  | Const _ -> true
  | _ -> false

let const_bool b = Const (B b)
let true_ = const_bool true
let false_ = const_bool false
let const_int i = Const (I i)
let key key = Key key

let not_ (x : bool t) : bool t =
  match x with
  | Const B b -> Const (B (not b))
  | _ -> Not x

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  match left, right with
  | Const cx, Const cy -> Const (Typed_binop.to_arithmetic binop cx cy)
  | _ -> Binop (binop, left, right)

module Solve (Expr : Z3_intf.S) = struct
  let binop_to_z3_expr (type a b) (binop : (a * a * b) Typed_binop.t) : a Expr.t -> a Expr.t -> b Expr.t =
    match binop with
    | Plus -> Expr.plus
    | Minus -> Expr.minus
    | Times -> Expr.times
    | Divide -> Expr.divide
    | Modulus -> Expr.modulus
    | Less_than -> Expr.less_than
    | Less_than_eq -> Expr.less_than_eq
    | Greater_than -> Fn.flip Expr.less_than (* note the flip *)
    | Greater_than_eq -> Fn.flip Expr.less_than_eq (* note the flip *)
    | Equal_int -> Expr.eq_ints
    | Equal_bool -> Expr.eq_bools
    | Not_equal -> Expr.neq
    | And -> Expr.and_
    | Or -> Expr.or_

  let rec to_formula : type a. a t -> a Expr.t = function
    | Const (I i) -> Expr.box_int i
    | Const (B b) -> Expr.box_bool b
    | Key k -> Expr.var_of_key k
    | Not y -> Expr.not_ (to_formula y)
    | Binop (binop, e1, e2) -> binop_to_z3_expr binop (to_formula e1) (to_formula e2)
end
