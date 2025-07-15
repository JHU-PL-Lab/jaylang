
open Core

module Key = Interp_common.Key.Stepkey

module Const = struct
  type _ t =
    | I : int -> int t
    | B : bool -> bool t

  let box_int i = I i
  let box_bool b = B b

  let equal (type a) (x : a t) (y : a t) : bool =
    match x, y with
    | I a, I b -> a = b
    | B a, B b -> Bool.(a = b)
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

  (* let equal (type a) (x : a t) (y : a t) : bool =
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
    | _ -> false *)
end

type _ t =
  | Const : 'a Const.t -> 'a t
  (* The following constructors form abstract expressions *)
  | Key : 'a Key.t -> 'a t
  | Not : bool t -> bool t
  | Binop : ('a * 'a * 'b) Typed_binop.t * 'a t * 'a t -> 'b t

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
  | Not e -> e (* cancel a double negation *)
  | _ -> Not x

let rec equal : type a. a t -> a t -> bool =
  fun x y ->
    match x, y with
    | Const c1, Const c2 -> Const.equal c1 c2
    | Key k1, Key k2 -> Key.equal k1 k2
    | Not e1, Not e2 -> equal e1 e2
    | Binop (op1, l1, r1), Binop (op2, l2, r2) -> begin
      match op1, op2 with
      | Plus, Plus -> equal l1 l2 && equal r1 r2
      | Minus, Minus -> equal l1 l2 && equal r1 r2
      | Times, Times -> equal l1 l2 && equal r1 r2
      | Divide, Divide -> equal l1 l2 && equal r1 r2
      | Modulus, Modulus -> equal l1 l2 && equal r1 r2
      | Less_than, Less_than -> equal l1 l2 && equal r1 r2
      | Less_than_eq, Less_than_eq -> equal l1 l2 && equal r1 r2
      | Greater_than, Greater_than -> equal l1 l2 && equal r1 r2
      | Greater_than_eq, Greater_than_eq -> equal l1 l2 && equal r1 r2
      | Equal_int, Equal_int -> equal l1 l2 && equal r1 r2
      | Equal_bool, Equal_bool -> equal l1 l2 && equal r1 r2
      | Not_equal, Not_equal -> equal l1 l2 && equal r1 r2
      | And, And -> equal l1 l2 && equal r1 r2
      | Or, Or -> equal l1 l2 && equal r1 r2
      | _ -> false
    end
    | _ -> false

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  match left, right, binop with
  | e, Const B b, And -> if b then e else Const (B false)
  | Const B b, e, And -> if b then e else Const (B false) (* combining with above doesn't typecheck, annoyingly *)
  | e, e', And when equal e (Not e') -> Const (B false)
  | e, e', And when equal e e' -> e
  | Const B b, e, Or -> if b then Const (B true) else e
  | e, Const B b, Or -> if b then Const (B true) else e (* .. *)
  | Const cx, Const cy, _ -> Const (Typed_binop.to_arithmetic binop cx cy)
  | Key k1, Key k2, Equal_bool when Key.equal k1 k2 -> Const (B true)
  | Key k1, Key k2, Equal_int when Key.equal k1 k2 -> Const (B true)
  | Key k1, Key k2, Not_equal when Key.equal k1 k2 -> Const (B false)
  | Key k, Const B true, Equal_bool -> Key k
  | Const B true, Key k, Equal_bool -> Key k
  | Key k, Const B false, Equal_bool -> not_ (Key k)
  | Const B false, Key k, Equal_bool -> not_ (Key k)
  | _ -> Binop (binop, left, right)

let rec subst : type a b. a t -> b Key.t -> b Const.t -> a t =
  fun e k c ->
    match e with
    | Key k' -> begin
      match k, k' with
      | I ik, I ik' when Interp_common.Step.equal ik ik' -> Const c
      | B bk, B bk' when Interp_common.Step.equal bk bk' -> Const c
      | _ -> e
    end
    | Const _ -> e
    | Not e' -> not_ @@ subst e' k c
    | Binop (bop, e1, e2) -> op (subst e1 k c) (subst e2 k c) bop

module Subst = Utils.Pack.Make (struct
  type 'a t = 'a Key.t * 'a
  let compare compare_a (k1, a1) (k2, a2) =
    match Key.compare k1 k2 with
    | 0 -> compare_a a1 a2
    | c -> c
end)

(*
  We would also like to notice when two keys must be equal, and then we
  substitute those through. This would help a lot with the formulas we
  get from deterministic functions.

  Instead of substitutions, it would be a good idea to use union-find
  to group keys and values. Some constant is always the representative if
  it exists. Otherwise choose the smallest key. The moment there are two different 
  constants (i.e. a conflict for representative), then we have an unsat formula.
  We would probably like to flatten all "ands" to lists of expressions, and that
  way we have the most simple expressions possible. We look at them for any
  equality at the top of the formula, and add to the equivalence classes if possible.
  Then we can substitute through with the representative. It may or may not be
  good to substitute through because it might be slow, but it also might reveal
  an unsat formula or constant formula.

  No bool is any good if we don't branch on it, which means we'll eventually want
  to simplify its formulas. Might as well do it early to only do it once.

  Problem with an out of the box union find is we don't have control over the
  representative. I want to be able to choose the representative to be a concrete value.
*)
let is_trivial (e : bool t) : [ `Trivial of Subst.t | `Nontrivial | `Const of bool ] =
  match e with
  | Const (B b) -> `Const b
  | Binop (Equal_int, Key k, Const I i) -> `Trivial (I (k, i))
  | Binop (Equal_bool, Key k, Const B b) -> `Trivial (B (k, b))
  | Binop (Equal_int, Const I i, Key k) -> `Trivial (I (k, i))
  | Binop (Equal_bool, Const B b, Key k) -> `Trivial (B (k, b))
  | Key k -> `Trivial (B (k, true))
  | Not Key k -> `Trivial (B (k, false))
  | _ -> `Nontrivial

let[@landmark] simplify (ls : bool t list) : Subst.t list * bool t list =
  let sub_list : type a b. a t list -> b Key.t -> b -> a t list =
    fun l k v ->
      match k with
      | I _ -> List.map l ~f:(fun e -> subst e k (I v))
      | B _ -> List.map l ~f:(fun e -> subst e k (B v))
  in
  let rec loop is_simplified acc_exprs acc_subs = function
  | [] -> 
    if is_simplified
    then loop false [] acc_subs acc_exprs (* still working on simplifying--loop aagin *)
    else Ok (acc_exprs, acc_subs) (* the simplification did nothing, so return *)
  | hd :: tl ->
    (* look for trivial substitutions we can make using this expression *)
    match is_trivial hd with
    | `Trivial (I (k, v) as sub) ->
      loop true (sub_list acc_exprs k v) (sub :: acc_subs) (sub_list tl k v)
    | `Trivial (B (k, v) as sub) ->
      loop true (sub_list acc_exprs k v) (sub :: acc_subs) (sub_list tl k v)
    | `Const b ->
      if b
      then loop is_simplified acc_exprs acc_subs tl
      else Error `Unsat
    | `Nontrivial ->
      loop is_simplified (hd :: acc_exprs) acc_subs tl
  in
  match loop false [] [] ls with
  | Error `Unsat -> [], [ Const (B false) ]
  | Ok (acc_exprs, acc_subs) -> acc_subs, acc_exprs

module Solve (Expr : Z3_api.S) = struct
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
