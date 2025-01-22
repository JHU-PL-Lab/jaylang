
open Core

module SuduZ3 = Sudu.Gadt_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

include SuduZ3

module Solve_status = struct
  type t =
    | Sat of Z3.Model.model
    | Unknown
    | Unsat
end

let solver = Z3.Solver.mk_solver ctx None

let get_int_expr model key =
  key
  |> Concolic_key.uniq_id
  |> int_var
  |> int_of_expr model

let get_bool_expr model key =
  key
  |> Concolic_key.uniq_id
  |> bool_var
  |> bool_of_expr model

let set_timeout time =
  time
  |> Time_float.Span.to_ms
  |> Float.iround_up_exn
  |> Int.to_string
  |> Z3.Params.update_param_value ctx "timeout"

let solve bool_formulas =
  Z3.Solver.add solver (SuduZ3.Gexpr.extract_list bool_formulas);
  let res = Z3.Solver.check solver [] in
  match res with
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Solver.get_model solver in
    Z3.Solver.reset solver;
    Solve_status.Sat (Option.value_exn model)
  | _ -> Z3.Solver.reset solver; Unsat