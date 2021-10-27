open Core

let ctx = Z3.mk_context []

module SuduZ3 = Sudu.Z3_api.Make (struct
  let ctx = ctx
end)

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

let get_inputs target_x model (target_stack : Concrete_stack.t) program =
  let input_history = ref [] in
  let input_feeder =
    memorized_solution_input_feeder input_history model target_stack
  in
  let target = (target_x, target_stack) in
  let _ = Naive_interpreter.eval ~input_feeder ~target program in
  List.rev !input_history

let get_cvar_picked model cvar_complete =
  Hashtbl.mapi
    ~f:(fun ~key:cvar ~data:_cc ->
      cvar.Cvar.picked_name |> SuduZ3.mk_bool_s |> SuduZ3.get_bool model
      |> Option.value ~default:false)
    cvar_complete

let cvar_complete_to_z3 cvar b =
  SuduZ3.eq
    (cvar.Cvar.complete_name |> SuduZ3.mk_bool_s)
    (Z3.Boolean.mk_val ctx b)

let cvar_complete_false_to_z3 cvar_complete_false =
  List.map cvar_complete_false ~f:(fun cvar -> cvar_complete_to_z3 cvar false)
