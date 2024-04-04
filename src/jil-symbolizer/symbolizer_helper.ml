open Core
open Dj_common

type result_info = { model : Z3.Model.model; c_stk : Concrete_stack.t }

exception Found_solution of result_info

type eg_edge =
  | K of (Lookup_key.t * Lookup_key.t)
  | K2 of (Lookup_key.t * Lookup_key.t)
  | Z of (Lookup_key.t * Z3.Expr.expr)
  | D of (Lookup_key.t * Lookup_key.t list)
  | P of Lookup_key.t
  | Phi of Z3.Expr.expr
