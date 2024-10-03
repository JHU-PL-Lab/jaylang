open Core
open Jayil.Ast

module Solve_status =
  struct
    type t =
      | Sat of Z3.Model.model
      | Unknown
      | Unsat
  end

module SuduZ3 = Sudu.Simpler_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

open SuduZ3
module Sort = Sudu.Simpler_z3_api.Sort
open Sort

let ctx = SuduZ3.ctx
let solver = Z3.Solver.mk_solver ctx None

let set_timeout time =
  time
  |> Time_float.Span.to_ms
  |> Float.iround_up_exn
  |> Int.to_string
  |> Z3.Params.update_param_value ctx "timeout"

let key_to_var (key : Concolic_key.t) (sort : Sort.t) : Z3.Expr.expr =
  SuduZ3.var sort @@ Concolic_key.x key

let solve formulas =
  Z3.Solver.add solver formulas;
  let res = Z3.Solver.check solver [] in
  match res with
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Solver.get_model solver in
    Z3.Solver.reset solver;
    Solve_status.Sat (Option.value_exn model)
  | _ -> Z3.Solver.reset solver; Unsat

let get_int_expr model key =
  SuduZ3.get_int_expr model
  @@ key_to_var key Int_sort

(* AST primitive *)

let not_ k1 k2 =
  fn_not (key_to_var k1 Bool_sort) (key_to_var k2 Bool_sort)

let sorts_of_binop = function
  | Binary_operator_plus
  | Binary_operator_minus
  | Binary_operator_times
  | Binary_operator_divide
  | Binary_operator_modulus -> Some (Int_sort, Int_sort, Int_sort)
  | Binary_operator_less_than
  | Binary_operator_less_than_or_equal_to
  | Binary_operator_not_equal_to -> Some (Bool_sort, Int_sort, Int_sort)
  | Binary_operator_or
  | Binary_operator_and -> Some (Bool_sort, Bool_sort, Bool_sort)
  | Binary_operator_equal_to -> None

let binop t op t1 left_v t2 =
  let open Jayil.Ast in
  let e_sort, e1_sort, e2_sort = 
    Option.value ~default:(let s = Dvalue.to_sort_exn left_v in Bool_sort, s, s)
    @@ sorts_of_binop op (* only None on binop equal to *)
  in
  let e = key_to_var t e_sort in
  let e1 = key_to_var t1 e1_sort in
  let e2 = key_to_var t2 e2_sort in
  let fop =
    match op with
    | Binary_operator_plus -> fn_plus
    | Binary_operator_minus -> fn_minus
    | Binary_operator_times -> fn_times
    | Binary_operator_divide -> fn_divide
    | Binary_operator_modulus -> fn_modulus
    | Binary_operator_less_than -> fn_lt
    | Binary_operator_less_than_or_equal_to -> fn_le
    | Binary_operator_equal_to -> fn_eq
    | Binary_operator_not_equal_to -> fn_neq (* TODO: This might be buggy. Check later *)
    | Binary_operator_and -> fn_and
    | Binary_operator_or -> fn_or
  in
  fop e e1 e2

let phi_and_sort_of_value = function
  | Value_int i -> SuduZ3.int_ i, Int_sort
  | Value_bool i -> SuduZ3.bool_ i, Bool_sort
  | Value_function _ 
  | Value_record _ -> failwith "z3 value of function or record is not allowed or supported" 

let eqv key v = let phi, sort = phi_and_sort_of_value v in SuduZ3.eq (key_to_var key sort) phi
let eq dv key key' = let s = Dvalue.to_sort_exn dv in SuduZ3.eq (key_to_var key s) (key_to_var key' s)
