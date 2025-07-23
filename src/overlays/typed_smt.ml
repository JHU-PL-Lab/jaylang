
open Core

module Binop = struct
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

  (* let equal (type a)(x : a t) (y : a t) : bool =
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
    | Equal, Equal
    | Not_equal, Not_equal
    | And, And
    | Or, Or -> true
    | _ -> false *)
end

open Binop

module type KEY = sig
  type t
  val uid : t -> int
end

module Symbol = struct
  module X = Utils.Separate.Make_with_compare (Int)
  type ('a, 'k) t = 'a X.t (* should be private *)

  let equal = X.equal

  let make_int (k : 'k) (uid : 'k -> int) : (int, 'k) t =
    I (uid k)

  let make_bool (k : 'k) (uid : 'k -> int) : (bool, 'k) t =
    B (uid k)
end

module Make_symbol (Key : KEY) = struct
  type 'a t = ('a, Key.t) Symbol.t

  let make_int (k : Key.t) : int t =
    Symbol.make_int k Key.uid

  let make_bool (k : Key.t) : bool t =
    Symbol.make_bool k Key.uid
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

module Model = struct
  type 'k t = { value : 'a. ('a, 'k) Symbol.t -> 'a option }

  let empty : 'k t =
    { value = fun _ -> None }
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

module type CONTEXT = sig
  val ctx : Z3.context
end

(*
  Z3 expressions using some context.

  We will often like to avoid the context, so I have my own
  homebrewed expressions further below that are functional.

  TODO: does this need to be generative, or is it fine to share contexts
    amongst functor applications?
*)
module Make_Z3 (C : CONTEXT) : SOLVABLE = struct
  (* I'm relying on internal correctness, and the types are phantom *)
  type ('a, 'k) t = Z3.Expr.expr (* will need to be private *)

  let ctx = C.ctx

  let equal = Z3.Expr.equal

  let const_int (i : int) : (int, 'k) t = Z3.Arithmetic.Integer.mk_numeral_i ctx i
  let const_bool (b : bool) : (bool, 'k) t = Z3.Boolean.mk_val ctx b

  let zero = const_int 0
  let one = const_int 1

  let intS = Z3.Arithmetic.Integer.mk_sort ctx
  let boolS = Z3.Boolean.mk_sort ctx

  let symbol (type a) (s : (a, 'k) Symbol.t) : (a, 'k) t =
    match s with
    | I k -> Z3.Expr.mk_const ctx (Z3.Symbol.mk_int ctx k) intS
    | B k -> Z3.Expr.mk_const ctx (Z3.Symbol.mk_int ctx k) boolS

  let not_ (e : (bool, 'k) t) : (bool, 'k) t =
    Z3.Boolean.mk_not ctx e

  let list_curry f x y = f [ x ; y ]

  let divides a b =
    Z3.Boolean.mk_eq ctx (const_int 0) (Z3.Arithmetic.Integer.mk_mod ctx b a)

  let rec binop : type a b. (a * a * b) Binop.t -> (a, 'k) t -> (a, 'k) t -> (b, 'k) t = fun op ->
    match op with
    | Plus            -> list_curry @@ Z3.Arithmetic.mk_add ctx
    | Minus           -> list_curry @@ Z3.Arithmetic.mk_sub ctx
    | Times           -> list_curry @@ Z3.Arithmetic.mk_mul ctx
    | Less_than       -> Z3.Arithmetic.mk_lt ctx
    | Less_than_eq    -> Z3.Arithmetic.mk_le ctx
    | Greater_than    -> Z3.Arithmetic.mk_gt ctx
    | Greater_than_eq -> Z3.Arithmetic.mk_ge ctx
    | Equal           -> Z3.Boolean.mk_eq ctx
    | Not_equal       -> fun a b -> not_ (Z3.Boolean.mk_eq ctx a b)
    | Or              -> list_curry @@ Z3.Boolean.mk_or ctx
    (* OCaml division and modulus differ from Z3, so we need some extra encoding *)
    | Divide -> fun x y ->
      let div = Z3.Arithmetic.mk_div ctx x y in
      Z3.Boolean.mk_ite ctx
        (binop Or (divides y x) (binop Less_than_eq zero x))
        div
        (Z3.Boolean.mk_ite ctx
          (binop Less_than_eq zero y)
          (binop Plus div one)
          (binop Minus div one)
      )
    | Modulus -> fun x y ->
      binop Minus x (binop Times x (binop Divide x y))

  let is_const (type a) (x : (a, 'k) t) : bool =
    Z3.Expr.is_const x

  let and_ (exprs : (bool, 'k) t list) : (bool, 'k) t =
    Z3.Boolean.mk_and ctx exprs

  let solver = Z3.Solver.mk_simple_solver ctx

  let unbox_int_expr e =
    Big_int_Z.int_of_big_int
    @@ Z3.Arithmetic.Integer.get_big_int e

  let unbox_bool_expr e =
    match Z3.Boolean.get_bool_value e with
    | L_FALSE -> false
    | L_TRUE -> true
    | L_UNDEF -> failwith "Invariant failure: unboxing non-bool into bool."

  (* TODO: is `eval` necessary? *)
  let a_of_expr z3_model expr unbox_expr =
    let open Option.Let_syntax in
    Z3.Model.get_const_interp_e z3_model expr
    >>= fun e -> Z3.Model.eval z3_model e false
    >>| unbox_expr

  let solve (exprs : (bool, 'k) t list) : 'k solution =
    match Z3.Solver.check solver [ and_ exprs ] with
    | Z3.Solver.SATISFIABLE ->
      let model = Option.value_exn @@ Z3.Solver.get_model solver in
      let value : type a. (a, 'k) Symbol.t -> a option = fun s ->
        match s with
        | I _ -> a_of_expr model (symbol s) unbox_int_expr
        | B _ -> a_of_expr model (symbol s) unbox_bool_expr
      in
      Sat { value }
    | UNKNOWN -> Unknown
    | UNSATISFIABLE -> Unsat
end

(*
  Home brewed expressions with some simplification.

  These require no context and are functional, so they
  are the default interface that everything should use.
*)
module T = struct
  type (_, 'k) t =
    | Const_int : int -> (int, 'k) t
    | Const_bool : bool -> (bool, 'k) t
    | Key : ('a, 'k) Symbol.t -> ('a, 'k) t
    | Not : (bool, 'k) t -> (bool, 'k) t
    | And : (bool, 'k) t list -> (bool, 'k) t
    | Binop : ('a * 'a * 'b) Binop.t * ('a, 'k) t * ('a, 'k) t -> ('b, 'k) t

  (* Polymorphic equality is good enough here because keys just use ints
    underneath. I would only write structural equality anyways. *)
  let equal a b = 
    Core.phys_equal a b
    || Core.Poly.equal a b

  let const_int i = Const_int i
  let const_bool b = Const_bool b
  let symbol s = Key s

  let true_ = Const_bool true
  let false_ = Const_bool false

  let is_const (type a) (x : (a, 'k) t) : bool =
    match x with
    | Const_int _ | Const_bool _ -> true
    | Key _ | Not _ | And _ | Binop _ -> false

  let rec binop : type a b. (a * a * b) Binop.t -> (a, 'k) t -> (a, 'k) t -> (b, 'k) t = fun op x y ->
    match op with
    | Or -> begin
        match x, y with
        | Const_bool true, _ -> Const_bool true
        | _, Const_bool true -> Const_bool true
        | Const_bool false, e -> e
        | e, Const_bool false -> e
        | e1, e2 -> Binop (Or, e1, e2)
      end
    | Equal -> begin
        match x, y with
        | Const_bool true, Key k -> Key k
        | Key k, Const_bool true -> Key k
        | Const_bool false, Key k -> Not (Key k)
        | Key k, Const_bool false -> Not (Key k)
        | Key k1, Key k2 when Symbol.equal k1 k2 -> Const_bool true
        | Const_bool b1, Const_bool b2 -> Const_bool (Bool.equal b1 b2)
        | Const_int i1, Const_int i2 -> Const_bool (i1 = i2)
        | e1, e2 -> Binop (Equal, e1, e2)
      end
    | Not_equal -> not_ (binop Equal x y)
    | Plus -> begin
        match x, y with
        | e, Const_int 0
        | Const_int 0, e -> e
        | Const_int i1, Const_int i2 -> Const_int (i1 + i2)
        | e1, e2 -> Binop (Plus, e1, e2)
      end
    | Minus -> begin
        match x, y with
        | e, Const_int 0 -> e
        | Const_int i1, Const_int i2 -> Const_int (i1 - i2)
        | e1, e2 -> Binop (Minus, e1, e2)
      end
    | Times -> begin
        match x, y with
        | e, Const_int 1
        | Const_int 1, e -> e
        | Const_int i1, Const_int i2 -> Const_int (i1 * i2)
        | e1, e2 -> Binop (Times, e1, e2)
      end
    | Divide -> begin
        match x, y with
        | e, Const_int 1 -> e
        | Const_int i1, Const_int i2 -> Const_int (i1 / i2)
        | e1, e2 -> Binop (Divide, e1, e2)
      end
    | Modulus -> begin
        match x, y with
        | Const_int i1, Const_int i2 -> Const_int (i1 mod i2)
        | e1, e2 -> Binop (Modulus, e1, e2)
      end
    | Less_than -> begin
        match x, y with
        | Const_int i1, Const_int i2 -> Const_bool (i1 < i2)
        | e1, e2 -> Binop (Less_than, e1, e2)
      end
    | Less_than_eq -> begin
        match x, y with
        | Const_int i1, Const_int i2 -> Const_bool (i1 <= i2)
        | e1, e2 -> Binop (Less_than_eq, e1, e2)
      end
    | Greater_than -> begin
        match x, y with
        | Const_int i1, Const_int i2 -> Const_bool (i1 > i2)
        | e1, e2 -> Binop (Greater_than, e1, e2)
      end
    | Greater_than_eq -> begin
        match x, y with
        | Const_int i1, Const_int i2 -> Const_bool (i1 >= i2)
        | e1, e2 -> Binop (Greater_than_eq, e1, e2)
    end

  and not_ (e : (bool, 'k) t) : (bool, 'k) t =
    match e with
    | Const_bool b -> Const_bool (not b)
    | Not e' -> e'
    | Binop (Or, e1, e2) -> and_ [ not_ e1 ; not_ e2 ] (* t's easier to work with "and" later *)
    | _ -> Not e

  (* Consider here checking if any is the negation of another *)
  and and_ (e_ls : (bool, 'k) t list) : (bool, 'k) t =
    match e_ls with
    | [] -> true_ (* vacuous truth *)
    | [ e ] -> e
    | hd :: tl ->
      match hd with
      | Const_bool true -> and_ tl
      | Const_bool false -> false_
      | And e_ls' -> and_ (e_ls' @ tl)
      | e -> 
        match and_ tl with
        | Const_bool false -> false_
        | Const_bool true -> e
        | And tl_exprs when List.exists tl_exprs ~f:(equal (not_ e)) -> false_
        | And tl_exprs when List.exists tl_exprs ~f:(equal e) -> And tl_exprs
        | And tl_exprs -> And (e :: tl_exprs)
        | other when equal other (not_ e) -> false_
        | other when equal other e -> e
        | other -> And [ e ; other ]
end

include T

module Transform (X : S) = struct
  let rec transform : type a. (a, 'k) t -> (a, 'k) X.t = fun e ->
    match e with
    | Const_int i -> X.const_int i
    | Const_bool b -> X.const_bool b
    | Key s -> X.symbol s
    | Not e' -> X.not_ (transform e')
    | And e_ls -> X.and_ (List.map e_ls ~f:transform)
    | Binop (op, e1, e2) -> X.binop op (transform e1) (transform e2)
end

(*
  TODO: add simplification as a preprocess.
*)
module Solve (X : SOLVABLE) = struct
  module M = Transform (X)

  let solve (exprs : (bool, 'k) t list) : 'k solution =
    X.solve @@ List.map exprs ~f:M.transform
end
