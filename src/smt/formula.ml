
open Core

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

module type SOLVABLE = sig
  include S

  val solve : (bool, 'k) t list -> 'k Solution.t
end

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
  | Binop (Or, e1, e2) -> and_ [ not_ e1 ; not_ e2 ] (* it's easier to work with "and" later *)
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

module Make_transformer (X : S) = struct
  let rec transform : type a. (a, 'k) t -> (a, 'k) X.t = fun e ->
    match e with
    | Const_int i -> X.const_int i
    | Const_bool b -> X.const_bool b
    | Key s -> X.symbol s
    | Not e' -> X.not_ (transform e')
    | And e_ls -> X.and_ (List.map e_ls ~f:transform)
    | Binop (op, e1, e2) -> X.binop op (transform e1) (transform e2)
end

module Make_solver (X : SOLVABLE) = struct
  module M = Make_transformer (X)

  let solve (exprs : (bool, 'k) t list) : 'k Solution.t =
    X.solve @@ List.map exprs ~f:M.transform
end
