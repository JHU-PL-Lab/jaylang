
open Core

(* module Untyped_binop = struct
  type t =
    | Plus
    | Minus
    | Times
    | Divide
    | Modulus
    | Less_than
    | Less_than_eq
    | Equal_int
    | Equal_bool
    | Not_equal
    | And
    | Or
end *)

module Typed_binop = struct
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

module T = struct
  type _ t =
    | Const_bool : bool -> bool t
    | Const_int : int -> int t 
    | Abstract_int : int e -> int t
    | Abstract_bool : bool e -> bool t

  (* abstract expressions only *)
  and _ e =
    | Bool_key : Concolic_key.t -> bool e
    | Int_key : Concolic_key.t -> int e
    | Not : bool t -> bool e
    | Plus : int t * int t -> int e
    | Minus : int t * int t -> int e
    | Times : int t * int t -> int e
    | Divide : int t * int t -> int e
    | Modulus : int t * int t -> int e
    | Less_than : int t * int t -> bool e
    | Less_than_eq : int t * int t -> bool e
    | Greater_than : int t * int t -> bool e
    | Greater_than_eq : int t * int t -> bool e
    | Equal_int : int t * int t -> bool e
    | Equal_bool : bool t * bool t -> bool e
    | Not_equal : int t * int t -> bool e
    | And : bool t * bool t -> bool e
    | Or : bool t * bool t -> bool e
end

include T

let is_const : type a. a t -> bool = function
  | Const_bool _
  | Const_int _ -> true
  | _ -> false

let simplify (type a b) (construct : a t -> a t -> b t) (op : a -> a -> b t) (x : a t) (y : a t) : b t =
  match x, y with
  | Const_int xi, Const_int yi -> op xi yi
  | Const_bool xb, Const_bool yb -> op xb yb
  | _ -> construct x y

let simplify_int op construct =
  simplify (fun x y -> Abstract_int (construct x y)) (fun x y -> Const_int (op x y))

let simplify_bool op construct =
  simplify (fun x y -> Abstract_bool (construct x y)) (fun x y -> Const_bool (op x y))

let true_ = Const_bool true
let const_bool b = Const_bool b
let const_int i = Const_int i
let bool_key key = Abstract_bool (Bool_key key)
let int_key key = Abstract_int (Int_key key)

let not_ (x : bool t) : bool t = match x with Const_bool b -> Const_bool (not b) | _ -> Abstract_bool (Not x)
let plus = simplify_int ( + ) (fun a b -> Plus (a, b))
let minus = simplify_int ( - ) (fun a b -> Minus (a, b))
let times = simplify_int ( * ) (fun a b -> Times (a, b))
let divide = simplify_int ( / ) (fun a b -> Divide (a, b))
let modulus = simplify_int ( mod ) (fun a b -> Modulus (a, b))
let less_than = simplify_bool ( < ) (fun a b -> Less_than (a, b))
let less_than_eq = simplify_bool ( <= ) (fun a b -> Less_than_eq (a, b))
let greater_than = simplify_bool ( > ) (fun a b -> Greater_than (a, b))
let greater_than_eq = simplify_bool ( >= ) (fun a b -> Greater_than_eq (a, b))
let equal_int = simplify_bool ( = ) (fun a b -> Equal_int (a, b))
let equal_bool = simplify_bool Bool.( = ) (fun a b -> Equal_bool (a, b))
let not_equal = simplify_bool ( <> ) (fun a b -> Not_equal (a, b))
let and_ = simplify_bool ( && ) (fun a b -> And (a, b))
let or_ = simplify_bool ( || ) (fun a b -> Or (a, b))

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  let f : a t -> a t -> b t = 
    match binop with
    | Plus -> plus
    | Minus -> minus
    | Times -> times
    | Divide -> divide
    | Modulus -> modulus
    | Less_than -> less_than
    | Less_than_eq -> less_than_eq
    | Greater_than -> greater_than
    | Greater_than_eq -> greater_than_eq
    | Equal_int -> equal_int
    | Equal_bool -> equal_bool
    | Not_equal -> not_equal
    | And -> and_
    | Or -> or_
  in
  f left right

module Resolve = struct
  (*
    I can't seem to get the type checker to like when I use polymorphic t to formula,
    but it works okay when I separately handle the int and bool cases.
  *)

  let rec int_t_to_formula (x : int t) : int C_sudu.Gexpr.t =
    match x with
    | Const_int i -> C_sudu.box_int i
    | Abstract_int ex -> int_e_to_formula ex

  and bool_t_to_formula (x : bool t) : bool C_sudu.Gexpr.t =
    match x with
    | Const_bool b -> C_sudu.box_bool b
    | Abstract_bool ex -> bool_e_to_formula ex

  and bool_e_to_formula (x : bool e) : bool C_sudu.Gexpr.t =
    let op_two_ints op e1 e2 = op (int_t_to_formula e1) (int_t_to_formula e2) in
    let op_two_bools op e1 e2 = op (bool_t_to_formula e1) (bool_t_to_formula e2) in
    match x with
    | Bool_key key -> C_sudu.bool_var @@ Concolic_key.uniq_id key
    | Not y -> C_sudu.not_ (bool_t_to_formula y)
    | Less_than (e1, e2) -> op_two_ints C_sudu.less_than e1 e2
    | Less_than_eq (e1, e2) -> op_two_ints C_sudu.less_than_eq e1 e2
    | Greater_than (e1, e2) -> op_two_ints C_sudu.less_than e2 e1 (* note the switch in order *)
    | Greater_than_eq (e1, e2) -> op_two_ints C_sudu.less_than_eq e2 e1 (* note the switch in order *)
    | Equal_int (e1, e2) -> op_two_ints C_sudu.eq_ints e1 e2
    | Equal_bool (e1, e2) -> op_two_bools C_sudu.eq_bools e1 e2
    | Not_equal (e1, e2) -> op_two_ints C_sudu.neq e1 e2
    | And (e1, e2) -> op_two_bools C_sudu.and_ e1 e2
    | Or (e1, e2) -> op_two_bools C_sudu.or_ e1 e2

  and int_e_to_formula (x : int e) : int C_sudu.Gexpr.t =
    let op_two_ints op e1 e2 = op (int_t_to_formula e1) (int_t_to_formula e2) in
    match x with
    | Int_key key -> C_sudu.int_var @@ Concolic_key.uniq_id key
    | Plus (e1, e2) -> op_two_ints C_sudu.plus e1 e2
    | Minus (e1, e2) -> op_two_ints C_sudu.minus e1 e2
    | Times (e1, e2) -> op_two_ints C_sudu.times e1 e2
    | Divide (e1, e2) -> op_two_ints C_sudu.divide e1 e2
    | Modulus (e1, e2) -> op_two_ints C_sudu.modulus e1 e2

  (* I can't combine the two int or two bool cases, or else the type checker thinks it's wrong. *)
  let t_to_formula (type a) (x : a t) : a C_sudu.Gexpr.t =
    match x with
    | Const_int _ -> int_t_to_formula x
    | Abstract_int _ -> int_t_to_formula x
    | Const_bool _ -> bool_t_to_formula x
    | Abstract_bool _ -> bool_t_to_formula x
end

let t_to_formula = Resolve.t_to_formula


