
open Core

module SuduZ3 = Sudu.Gadt_z3_api.Make (struct
  let ctx = Z3.mk_context []
end)

include SuduZ3

type 'a box = 'a -> 'a Gexpr.t

module Solve_status = struct
  type t =
    | Sat of Z3.Model.model
    | Unknown
    | Unsat
end

let solver = Z3.Solver.mk_solver ctx None

let var_of_key (type a) (key : a Stepkey.t) : a Gexpr.t =
  match key with
  | Int_key id -> int_var id
  | Bool_key id -> bool_var id

let value_of_key model key =
  key
  |> var_of_key
  |> value_of_expr model

let set_timeout time =
  time
  |> Time_float.Span.to_ms
  |> Float.iround_up_exn
  |> Int.to_string
  |> Z3.Params.update_param_value ctx "timeout"

let solve bool_formulas =
  Z3.Solver.add solver (SuduZ3.Gexpr.extract_list bool_formulas);
  (* Format.printf "Model is %s\n" (Z3.Solver.to_string solver); *)
  let res = Z3.Solver.check solver [] in
  match res with
  | Z3.Solver.SATISFIABLE ->
    let model = Z3.Solver.get_model solver in
    Z3.Solver.reset solver;
    Solve_status.Sat (Option.value_exn model)
  | _ -> Z3.Solver.reset solver; Unsat