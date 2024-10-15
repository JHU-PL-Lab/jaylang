open Core
open Dj_common.Log.Export

let ctx = Z3.mk_context []

let set_timeout_sec ctx sec =
  match sec with
  | None -> ()
  | Some sec ->
      let time_s =
        sec |> Time_float.Span.to_sec |> Float.iround_up_exn |> fun t ->
        t * 1000 |> string_of_int
      in
      Z3.Params.update_param_value ctx "timeout" time_s

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

type value = Sudu.Z3_api.plain =
  | Int of int [@printer Fmt.int]
  | Bool of bool [@printer Fmt.bool]
  | Fun of string [@printer Fmt.string]
  | Record of int [@printer Fmt.int] (* int is bitvector *)
[@@deriving sexp, compare, equal, show { with_path = false }]

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
  SuduZ3.check_with_assumption solver phi_used_once
