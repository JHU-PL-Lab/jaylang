open Core
open Z3

let time_work f =
  let start_time = Time_ns.now () in
  let result = f () in
  let end_time = Time_ns.now () in
  let span = Time_ns.(diff end_time start_time) in
  Printf.printf "{%f} " (Time_ns.Span.to_sec span);
  result

let print_result result = 
  (match result with
  | Result.Ok _model -> Printf.printf "SAT; "
  | Result.Error _ -> Printf.printf "UNSAT; ")

let ctx = Z3.mk_context []

module Z3API = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

let bool_sort = Boolean.mk_sort ctx
let vb i = Expr.mk_const_s ctx ("b" ^ string_of_int i) bool_sort

let solver = Z3.Solver.mk_solver ctx None