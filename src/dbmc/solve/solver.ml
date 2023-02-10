open Core
open Dj_common.Log.Export

let ctx = Z3.mk_context []

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

type value = Sudu.Z3_api.plain =
  | Int of int [@printer Fmt.int]
  | Bool of bool [@printer Fmt.bool]
  | Fun of string [@printer Fmt.string]
  | Record of string [@printer Fmt.string]
[@@deriving sexp, compare, equal, show { with_path = false }]

(* let solver = Z3.Solver.mk_solver ctx None *)
let reset solver = Z3.Solver.reset solver
let get_assertion_count solver = List.length (Z3.Solver.get_assertions solver)
let string_of_solver solver = Z3.Solver.to_string solver

let get_rlimit solver =
  let stat = Z3.Solver.get_statistics solver in
  let r = Z3.Statistics.get stat "rlimit count" in
  let rv = Option.value_exn r in
  Z3.Statistics.Entry.get_int rv

let check ?(verbose = true) solver phis phi_used_once =
  Z3.Solver.add solver phis ;

  if verbose
  then (
    SLog.debug (fun m ->
        m "Used-once Phis:@?@\n@[<v>%a@]"
          Fmt.(list ~sep:cut string)
          (List.map ~f:Z3.Expr.to_string phi_used_once)) ;
    SLog.debug (fun m ->
        m "Solver Phis (%d) : %s"
          (get_assertion_count solver)
          (string_of_solver solver)))
  else () ;

  SuduZ3.check_with_assumption solver phi_used_once

let check_expected_input_sat target_stk history solver =
  let input_phis =
    List.filter_map history ~f:(fun (x, stk, r) ->
        Option.map r ~f:(fun i ->
            let stk = Rstack.relativize target_stk stk in
            let name = Lookup_key.to_str2 x stk in
            let zname = SuduZ3.var_s name in
            SuduZ3.eq zname (SuduZ3.int_ i)))
  in

  Result.is_ok (SuduZ3.check_with_assumption solver input_phis)
