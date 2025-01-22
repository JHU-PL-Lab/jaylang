
open Core

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
    | Equal_int -> ( = )
    | Equal_bool -> Bool.( = )
    | Not_equal -> ( <> )
    | And -> ( && )
    | Or -> ( || )

  let to_z3_expr (type a b) (binop : (a * a * b) t) : a C_sudu.Gexpr.t -> a C_sudu.Gexpr.t -> b C_sudu.Gexpr.t =
    match binop with
    | Plus -> C_sudu.plus
    | Minus -> C_sudu.minus
    | Times -> C_sudu.times
    | Divide -> C_sudu.divide
    | Modulus -> C_sudu.modulus
    | Less_than -> C_sudu.less_than
    | Less_than_eq -> C_sudu.less_than_eq
    | Greater_than -> Fn.flip C_sudu.less_than (* note the flip *)
    | Greater_than_eq -> Fn.flip C_sudu.less_than_eq (* note the flip *)
    | Equal_int -> C_sudu.eq
    | Equal_bool -> C_sudu.eq
    | Not_equal -> C_sudu.neq
    | And -> C_sudu.and_
    | Or -> C_sudu.or_
end

type _ t =
  | Const : 'a -> 'a t
  | Abstract : 'a e -> 'a t

(* abstract expressions only *)
and _ e =
  | Bool_key : Concolic_key.t -> bool e
  | Int_key : Concolic_key.t -> int e
  | Not : bool t -> bool e
  | Binop : ('a * 'a * 'b) Typed_binop.t * 'a t * 'a t -> 'b e

let is_const : type a. a t -> bool = function
  | Const _ -> true
  | _ -> false

let true_ = Const true
let const_bool b = Const b
let const_int i = Const i
let bool_key key = Abstract (Bool_key key)
let int_key key = Abstract (Int_key key)

let not_ (x : bool t) : bool t = 
  match x with 
  | Const b -> Const (not b) 
  | _ -> Abstract (Not x)

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  match left, right with
  | Const cx, Const cy -> Const (Typed_binop.to_arithmetic binop cx cy)
  | _ -> Abstract (Binop (binop, left, right))

module Resolve = struct
  type 'a conv = 'a t -> 'a C_sudu.Gexpr.t

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
  let e_to_formula (type a) (i : int conv) (b : bool conv) (x : a e) : a C_sudu.Gexpr.t =
    match x with
    | Int_key key -> C_sudu.int_var @@ Concolic_key.uniq_id key
    | Bool_key key -> C_sudu.bool_var @@ Concolic_key.uniq_id key
    | Not y -> C_sudu.not_ (b y)
    | Binop (binop, e1, e2) ->
      let to_formula = binop_opkind_to_converter i b binop in
      Typed_binop.to_z3_expr binop (to_formula e1) (to_formula e2)

  let rec int_t_to_formula : int conv = function
    | Const i -> C_sudu.box_int i
    | Abstract ex -> int_e_to_formula ex

  and bool_t_to_formula : bool conv = function
    | Const b -> C_sudu.box_bool b
    | Abstract ex -> bool_e_to_formula ex

  and int_e_to_formula e = e_to_formula int_t_to_formula bool_t_to_formula e
  
  and bool_e_to_formula e = e_to_formula int_t_to_formula bool_t_to_formula e
end

include Resolve


