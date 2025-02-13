
open Core

module Const = struct
  type _ t =
    | I : int -> int t
    | B : bool -> bool t

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
  | Abstract : 'a e -> 'a t

(* abstract expressions only *)
and _ e =
  | Key : 'a Stepkey.t -> 'a e
  | Not : bool t -> bool e
  | Binop : ('a * 'a * 'b) Typed_binop.t * 'a t * 'a t -> 'b e

let is_const : type a. a t -> bool = function
  | Const _ -> true
  | _ -> false

let const_bool b = Const (B b)
let true_ = const_bool true
let false_ = const_bool false
let const_int i = Const (I i)
let key key = Abstract (Key key)

let not_ (x : bool t) : bool t =
  match x with
  | Const B b -> Const (B (not b))
  | _ -> Abstract (Not x)

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  match left, right with
  | Const cx, Const cy -> Const (Typed_binop.to_arithmetic binop cx cy)
  | _ -> Abstract (Binop (binop, left, right))

module Solve (Expr : Z3_intf.S) = struct
  type 'a conv = 'a t -> 'a Expr.t

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

  (* It's dumb how I cannot combine cases here *)
  let binop_opkind_to_converter (type a b) (i : int conv) (b : bool conv) (binop : (a * a * b) Typed_binop.t) : a conv =
    match binop with
    | Plus -> i
    | Minus -> i
    | Times -> i
    | Divide -> i
    | Modulus -> i
    | Less_than -> i
    | Less_than_eq -> i
    | Greater_than -> i
    | Greater_than_eq -> i
    | Equal_int -> i
    | Not_equal -> i
    | Equal_bool -> b
    | And -> b
    | Or -> b

  (*
    Because of issues with mutual recursion and locally abstract types, I have to do this
    weird hack where I pass in each "t_to_formula" converter.
  *)
  let e_to_formula (type a) (i : int conv) (b : bool conv) (x : a e) : a Expr.t =
    match x with
    | Key k -> Expr.var_of_key k
    | Not y -> Expr.not_ (b y)
    | Binop (binop, e1, e2) ->
      let to_formula = binop_opkind_to_converter i b binop in
      binop_to_z3_expr binop (to_formula e1) (to_formula e2)

  let rec t_to_formula : type a. a conv = function
    | Const (I i) -> Expr.box_int i
    | Const (B b) -> Expr.box_bool b
    | Abstract ex -> e_to_formula t_to_formula t_to_formula ex
end
