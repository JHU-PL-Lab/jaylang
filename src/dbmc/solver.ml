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
  Z3.Solver.add solver phis_z3;
  SuduZ3.check_with_assumption solver cvars_z3

let string_of_solver () = Z3.Solver.to_string solver

let solution_input_feeder model target_stack (x, call_stack) : int option =
  let stk = Rstack.relativize target_stack call_stack in
  let name = Lookup_key.parts2_to_str [ x ] stk in
  SuduZ3.get_int_s model name

let memorized_solution_input_feeder mem model target_stack =
  let input_feeder = solution_input_feeder model target_stack in
  fun query ->
    let answer = input_feeder query in
    mem := answer :: !mem;
    answer

let get_inputs ~(state : Global_state.t) ~(config : Top_config.t) target_x model
    (target_stack : Concrete_stack.t) program =
  let input_history = ref [] in
  let input_feeder =
    memorized_solution_input_feeder input_history model target_stack
  in
  let target = (target_x, target_stack) in
  let max_step = config.run_max_step in

  let _ =
    Naive_interpreter.eval ~state ~config ~input_feeder ~target ~max_step
      program
  in
  List.rev !input_history
