
open Core

module Untyped_binop =
struct
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
end

module T =
struct
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
let equal_int = simplify_bool ( = ) (fun a b -> Equal_int (a, b))
let equal_bool = simplify_bool Bool.( = ) (fun a b -> Equal_bool (a, b))
let not_equal = simplify_bool ( <> ) (fun a b -> Not_equal (a, b))
let and_ = simplify_bool ( && ) (fun a b -> And (a, b))
let or_ = simplify_bool ( || ) (fun a b -> Or (a, b))