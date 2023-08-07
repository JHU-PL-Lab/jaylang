open Core
open Dj_common

module Abs_exp = struct
  module T = struct
    type var = Var of Id.t [@@deriving equal, compare, hash, sexp]

    type binop = Jayil.Ast.binary_operator =
      | Binary_operator_plus
      | Binary_operator_minus
      | Binary_operator_times
      | Binary_operator_divide
      | Binary_operator_modulus
      | Binary_operator_less_than
      | Binary_operator_less_than_or_equal_to
      | Binary_operator_equal_to
      | Binary_operator_not_equal_to
      | Binary_operator_and
      | Binary_operator_or
    [@@deriving equal, compare, hash, sexp]

    type t = exp
    and exp = clause list
    and clause = Clause of Id.t * clause_body

    and clause_body =
      | Value of value
      | CVar of var
      | Appl of var * var
      | Not of var
      | Binop of var * binop * var
      | Restc

    and value = Int | Bool of bool | Function of var * exp | Restv
    [@@deriving equal, compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

include Abs_exp

let of_var (Jayil.Ast.Var (x, _)) = Var x
let id_of_var (Jayil.Ast.Var (x, _)) = x
let to_id (Var x) = x

let rec lift_expr (Jayil.Ast.Expr cls) = List.map ~f:lift_clause cls

and lift_clause (Jayil.Ast.Clause (x, cbody)) =
  Clause (id_of_var x, lift_cbody cbody)

and lift_cbody = function
  | Jayil.Ast.Value_body v -> Value (lift_value v)
  | Jayil.Ast.Var_body x -> CVar (of_var x)
  | Jayil.Ast.Input_body -> Value Int
  | Jayil.Ast.Appl_body (x1, x2) -> Appl (of_var x1, of_var x2)
  | Jayil.Ast.Not_body x -> Not (of_var x)
  | Jayil.Ast.Binary_operation_body (x1, bop, x2) ->
      Binop (of_var x1, bop, of_var x2)
  | _ -> Restc

and lift_value = function
  | Jayil.Ast.Value_int _ -> Int
  | Jayil.Ast.Value_bool b -> Bool b
  | Jayil.Ast.Value_function (Function_value (x, e)) ->
      Function (of_var x, lift_expr e)
  | _ -> Restv

let clb_to_string clb = Sexp.to_string_hum (sexp_of_clause_body clb)
