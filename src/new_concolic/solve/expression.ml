
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

  let to_arithmetic (type a b) (binop : (a * a * b) t) : (a -> a -> b) * b C_sudu.box =
    let open C_sudu in
    match binop with
    | Plus -> ( + ), box_int
    | Minus -> ( - ), box_int
    | Times -> ( * ), box_int
    | Divide -> ( / ), box_int
    | Modulus -> ( mod ), box_int
    | Less_than -> ( < ), box_bool
    | Less_than_eq -> ( <= ), box_bool
    | Greater_than -> ( > ), box_bool
    | Greater_than_eq -> ( >= ), box_bool
    | Equal_int -> ( = ), box_bool
    | Equal_bool -> Bool.( = ), box_bool
    | Not_equal -> ( <> ), box_bool
    | And -> ( && ), box_bool
    | Or -> ( || ), box_bool

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
  | Const : 'a * ('a -> 'a C_sudu.Gexpr.t) -> 'a t
  | Abstract : 'a e -> 'a t

(* abstract expressions only *)
and _ e =
  | Key : 'a Stepkey.t -> 'a e
  | Not : bool t -> bool e
  | Binop : ('a * 'a * 'b) Typed_binop.t * 'a t * 'a t -> 'b e

let is_const : type a. a t -> bool = function
  | Const _ -> true
  | _ -> false

let const_bool b = Const (b, C_sudu.box_bool)
let true_ = const_bool true
let const_int i = Const (i, C_sudu.box_int)
let key key = Abstract (Key key)

let not_ (x : bool t) : bool t = 
  match x with 
  | Const (b, box) -> Const (not b, box)
  | _ -> Abstract (Not x)

let op (type a b) (left : a t) (right : a t) (binop : (a * a * b) Typed_binop.t) : b t =
  match left, right with
  | Const (cx, _), Const (cy, _) -> let f, box = Typed_binop.to_arithmetic binop in Const (f cx cy, box)
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
  let e_to_formula (type a b) (i : int conv) (b : bool conv) (x : a e) : a C_sudu.Gexpr.t =
    match x with
    | Key key -> begin
      match key with
      | Int_key id -> C_sudu.int_var id
      | Bool_key id -> C_sudu.bool_var id
    end
    | Not y -> C_sudu.not_ (b y)
    | Binop (binop, e1, e2) ->
      let to_formula = binop_opkind_to_converter i b binop in
      Typed_binop.to_z3_expr binop (to_formula e1) (to_formula e2)

  let rec t_to_formula : type a. a conv = function
    | Const (c, f_box) -> f_box c
    | Abstract ex -> e_to_formula t_to_formula t_to_formula ex
end

include Resolve


