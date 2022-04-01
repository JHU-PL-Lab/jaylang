open Core
open Odefa_ast

type id = Id.t
type bop = Add | Sub | Mul | Div | Mod | Le | Leq | Eq | And | Or | Xor
(* | Not *)

type var = Var of id

type value = Record of record_val | Fun of fun_val | Int of int | Bool of bool
and record_val = var Map.M(Id).t
and fun_val = { arg : id; body : exp }
and pat = Pn_fun | Pn_int | Pn_bool | Pn_any | Pn_record of pat Map.M(Id).t
and exp = Clause of var * clause_body | Block of exp list

and clause_body =
  | Val of value
  | Var_body of var
  | Input
  | App of var * var
  | Cond of var * exp * exp
  | Match of var * pat
  | Proj of var * id
  | Binop of var * bop * var
  | Abort
  | Assume of var
  | Assert of var

let of_odefa_var (Odefa_ast.Ast.Var (id, _)) = Var id

let of_odefa_binop = function
  | Ast.Binary_operator_plus -> Add
  | Ast.Binary_operator_minus -> Sub
  | Ast.Binary_operator_times -> Mul
  | Ast.Binary_operator_divide -> Div
  | Ast.Binary_operator_modulus -> Mod
  | Ast.Binary_operator_less_than -> Le
  | Ast.Binary_operator_less_than_or_equal_to -> Leq
  | Ast.Binary_operator_equal_to -> Eq
  | Ast.Binary_operator_not_equal_to -> failwith "not supported yet"
  | Ast.Binary_operator_and -> And
  | Ast.Binary_operator_or -> Or
  | Ast.Binary_operator_xor -> Xor

let rec of_odefa_ast expr =
  let (Ast.Expr chauses) = expr in
  let es = List.map chauses ~f:of_odefa_clause in
  Block es

and of_odefa_val = function
  | Ast.Value_int i -> Int i
  | Ast.Value_bool b -> Bool b
  | _ -> failwith "TOOD: record"

and of_odefa_clause clause =
  let (Ast.Clause (Ast.Var (id, _), clause)) = clause in
  let clause_here =
    match clause with
    | Ast.Value_body value -> Val (of_odefa_val value)
    | Ast.Var_body x -> Var_body (of_odefa_var x)
    | Ast.Input_body -> Input
    | Ast.Appl_body (x1, x2) -> App (of_odefa_var x1, of_odefa_var x2)
    | Ast.Conditional_body (x, e1, e2) ->
        Cond (of_odefa_var x, of_odefa_ast e1, of_odefa_ast e2)
    | Ast.Match_body (x, _pat) -> Match (of_odefa_var x, Pn_fun)
    | Ast.Projection_body (x, id) -> Proj (of_odefa_var x, id)
    | Ast.Binary_operation_body (x1, op, x2) ->
        Binop (of_odefa_var x1, of_odefa_binop op, of_odefa_var x2)
    | Ast.Abort_body -> Abort
    | Ast.Assume_body x -> Assume (of_odefa_var x)
    | Ast.Assert_body x -> Assert (of_odefa_var x)
  in

  Clause (Var id, clause_here)
