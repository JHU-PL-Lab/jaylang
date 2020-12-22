open Core

module T = struct
  type value = 
    | Int of int
    | Bool of bool
    | Fun of Id.t
    | Record
  [@@deriving sexp, compare, equal]

  type binop =
    | Add | Sub | Mul | Div | Mod
    | Le | Leq | Eq
    | And | Or (* | Not *) | Xor
  [@@deriving sexp, compare, equal]

  type t = 
    | Eq_v of Symbol.t * value
    | Eq_x of Symbol.t * Symbol.t
    | Eq_lookup of Id.t list * Relative_stack.t * Id.t list * Relative_stack.t
    | Eq_binop of Symbol.t * Symbol.t * binop * Symbol.t
    | Eq_projection of Symbol.t * Symbol.t * Id.t
    | Target_stack of Relative_stack.t
    | C_and of t * t
    | C_exclusive of t list
  [@@deriving sexp, compare, equal]
end

include T
include Comparator.Make(T)

let to_string c =
  c |> sexp_of_t |> Sexp.to_string_hum

let list_to_string cs =
  cs |> [%sexp_of: t list] |> Sexp.to_string_hum

let to_smt_v =
  let open Odefa_ast.Ast in
  function
  | Value_int i -> Int i
  | Value_bool b -> Bool b
  | _ -> failwith "to_smt_v: not supported yet"  

let to_smt_op = 
  let open Odefa_ast.Ast in
  function
  | Binary_operator_plus -> Add
  | Binary_operator_minus -> Sub
  | Binary_operator_times -> Mul
  | Binary_operator_divide -> Div
  | Binary_operator_modulus -> Mod
  | Binary_operator_less_than -> Le
  | Binary_operator_less_than_or_equal_to -> Leq
  | Binary_operator_equal_to -> Eq
  | Binary_operator_and -> And
  | Binary_operator_or -> Or
  | Binary_operator_xor -> Xor

let bind_v x v stk = 
  Eq_v (Symbol.id x stk, to_smt_v v)

let bind_input x stk = 
  Eq_x (Symbol.id x stk, Symbol.id x stk)

let eq_lookup xs1 stk1 xs2 stk2 =
  Eq_lookup (xs1, stk1, xs2, stk2)

let bind_binop x y1 op y2 stk = 
  Eq_binop (Symbol.id x stk, 
            Symbol.id y1 stk, to_smt_op op, Symbol.id y2 stk)

let bind_fun x stk f =
  Eq_x (Symbol.id x stk, Symbol.funid f)

let concretize stk = Target_stack (Relative_stack.concretize stk)

let and_ c1 c2 = C_and (c1, c2)

let only_one cs = C_exclusive cs

