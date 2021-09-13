open Core

let ctx = Z3.mk_context []

module Z3API =
  Solver.Make
    (struct
      let ctx = ctx
    end)
    ()

let solver = Z3.Solver.mk_solver ctx None

let reset () = Z3.Solver.reset solver

let check phi_z3_list z3_gate_phis =
  Z3.Solver.add solver phi_z3_list;
  Z3API.check_with_assumption solver z3_gate_phis

let string_of_solver () = Z3.Solver.to_string solver

let solution_input_feeder model target_stack (x, call_stack) : int option =
  let x = x |> Id.of_ast_id in
  let call_stk = call_stack |> Concrete_stack.of_ast_id in
  let stk = Relative_stack.relativize target_stack call_stk in
  let sym = Symbol.id x stk in
  Z3API.get_int_s model (Symbol.show sym)

let memorized_solution_input_feeder mem model target_stack =
  let input_feeder = solution_input_feeder model target_stack in
  fun query ->
    let answer = input_feeder query in
    mem := answer :: !mem;
    answer

let get_inputs target_x model (target_stack : Concrete_stack.t) program =
  let input_history = ref [] in
  let input_feeder =
    memorized_solution_input_feeder input_history model target_stack
  in
  let target_stk = Concrete_stack.to_ast_id target_stack in
  let target = (target_x, target_stk) in
  let _ =
    Odefa_interpreter.Naive_interpreter.eval ~input_feeder ~target program
  in
  List.rev !input_history
