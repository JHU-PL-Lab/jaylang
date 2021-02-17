open Core
let ctx = Z3.mk_context []
module Z3API = Solver.Make (struct let ctx = ctx end) ()
let solver = Z3.Solver.mk_solver ctx None

let reset () =
  Z3.Solver.reset solver

let check phi_set gates =
  let z3_phis = List.map !phi_set ~f:Z3API.z3_phis_of_smt_phi in
  Z3.Solver.add solver z3_phis;
  phi_set := [];

  let z3_gate_phis = Z3API.z3_gate_phis gates in

  Z3API.check_with_assumption solver z3_gate_phis


let solution_input_feeder model target_stack =
  fun (x, call_stack) : int ->
  let x = x |> Id.of_ast_id in
  let call_stk = call_stack |> Concrete_stack.of_ast_id in 
  let stk = Relative_stack.relativize target_stack call_stk in
  let sym = Symbol.id x stk in
  Z3API.get_int_s model (Symbol.to_string_mach sym)

let memorized_solution_input_feeder mem model target_stack =
  let input_feeder = solution_input_feeder model target_stack in
  fun query ->
    let answer = input_feeder query in
    mem := answer :: !mem;
    answer

let get_input target_x model program = 
  let target_stack = Z3API.get_top_stack model in
  let input_history = ref [] in
  let input_feeder = memorized_solution_input_feeder input_history model target_stack in
  let target = (Id.to_ast_id target_x, Concrete_stack.to_ast_id target_stack) in
  let _ = Odefa_interpreter.Naive_interpreter.eval ~input_feeder ~target program in
  List.rev !input_history