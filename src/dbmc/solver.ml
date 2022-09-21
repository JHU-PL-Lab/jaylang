open Core

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

let solver = Z3.Solver.mk_solver ctx None
let reset () = Z3.Solver.reset solver

let check phis cvars_z3 =
  Z3.Solver.add solver phis ;
  SuduZ3.check_with_assumption solver cvars_z3

let string_of_solver () = Z3.Solver.to_string solver

let check_expected_input_sat target_stk history =
  let input_phis =
    List.filter_map history ~f:(fun (x, stk, r) ->
        Option.map r ~f:(fun i ->
            let stk = Rstack.relativize target_stk stk in
            let name = Lookup_key.to_str2 x stk in
            let zname = SuduZ3.var_s name in
            SuduZ3.eq zname (SuduZ3.int_ i)))
  in

  Result.is_ok (SuduZ3.check_with_assumption solver input_phis)
