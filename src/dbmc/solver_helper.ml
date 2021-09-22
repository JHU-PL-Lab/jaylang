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

let check phis_z3 cvars_z3 =
  Z3.Solver.add solver phis_z3;
  Z3API.check_with_assumption solver cvars_z3

let string_of_solver () = Z3.Solver.to_string solver

let solution_input_feeder model target_stack (x, call_stack) : int option =
  let stk = Relative_stack.relativize target_stack call_stack in
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
  let target = (target_x, target_stack) in
  let _ =
    Odefa_interpreter.Naive_interpreter.eval ~input_feeder ~target program
  in
  List.rev !input_history

let get_cvar_picked model cvar_complete =
  Hashtbl.mapi
    ~f:(fun ~key:cname ~data:_cc ->
      Cvar.set_picked cname |> Cvar.print |> Z3API.boole_of_str
      |> Z3API.get_bool model
      |> Option.value ~default:false)
    cvar_complete

let cvar_complete_to_z3 cvar b =
  Z3API.eq
    (Z3API.boole_of_str Cvar.(cvar |> set_complete |> print))
    (Z3.Boolean.mk_val ctx b)

let cvars_complete_to_z3 ccs =
  List.map ccs ~f:(fun (cvar, b) -> cvar_complete_to_z3 cvar b)

let cvar_complete_false_to_z3 cvar_complete_false =
  List.map cvar_complete_false ~f:(fun cvar -> cvar_complete_to_z3 cvar false)
