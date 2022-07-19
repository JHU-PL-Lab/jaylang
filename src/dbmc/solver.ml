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

let check phis_z3 cvars_z3 =
  Z3.Solver.add solver phis_z3 ;
  SuduZ3.check_with_assumption solver cvars_z3

let string_of_solver () = Z3.Solver.to_string solver
