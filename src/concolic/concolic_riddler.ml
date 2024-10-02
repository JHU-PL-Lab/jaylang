open Core
open Jayil.Ast

module Solve_status =
  struct
    type t =
      | Sat of Z3.Model.model
      | Unknown
      | Unsat
  end

module SuduZ3 = Sudu.Simple_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

open SuduZ3

let ctx = SuduZ3.ctx
let solver = Z3.Solver.mk_solver ctx None

let set_timeout time =
  time
  |> Time_float.Span.to_ms
  |> Float.iround_up_exn
  |> Int.to_string
  |> Z3.Params.update_param_value ctx "timeout"

let key_to_var key =
  SuduZ3.var_i
  @@ Concolic_key.x key

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
  @@ key_to_var key

(* AST primitive *)

let not_ t t1 =
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  fn_not e e1

let binop t op t1 t2 =
  let open Jayil.Ast in
  let e = key_to_var t in
  let e1 = key_to_var t1 in
  let e2 = key_to_var t2 in
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

let is_bool key = ifBool (key_to_var key)

let phi_of_value (_key : Concolic_key.t) = function
  | Value_int i -> SuduZ3.int_ i
  | Value_bool i -> SuduZ3.bool_ i
  | Value_function _ -> SuduZ3.fun_
  | Value_record _ -> SuduZ3.record_

let eqv key v = SuduZ3.eq (key_to_var key) (phi_of_value key v)
let eq key key' = SuduZ3.eq (key_to_var key) (key_to_var key')
