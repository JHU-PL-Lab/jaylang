(* open Core *)
(* this file will be moved to a separate module *)

open Dbmc_lib

let generate_inputs 
    test_generator
  : (int list * int) list * 'a option =
  (* let generation_callback inputs steps = prints results *)

  let inputs = [1;2] in
  let step = 3 in
  let answers = [(inputs, step)] in
  let generator_opt = Some test_generator in
  answers, generator_opt

(* 
type solution =
  (symbol -> Ast.value option) * Relative_stack.concrete_stack option
 *)

let build_input_sequence
    _solution
    _program
    _target
  : int list =
  []

let ctx = Z3.mk_context []

module Z3API = Dmbc_solver.Solver.Make (struct let ctx = ctx end) ()

let generate program target =
  let solver  = Z3.Solver.mk_solver ctx None in
  let phis = Odefa_symbolic_interpreter.Dbmc.lookup_main program target in
  print_endline @@ Constraint.list_to_string phis;
  let z3_phis = List.map Z3API.z3_phis_of_smt_phi phis in
  Z3.Solver.add solver z3_phis;
  print_endline @@ Z3.Solver.to_string solver;

  match Z3.Solver.check solver [] with
  | Z3.Solver.SATISFIABLE ->
    begin
      match Z3.Solver.get_model solver with
      | None -> print_endline "none"
      | Some model -> print_endline @@ Z3.Model.to_string model
    end
  | Z3.Solver.UNSATISFIABLE ->
    print_endline "UNSAT"
  | Z3.Solver.UNKNOWN ->
    failwith @@ Printf.sprintf "Unknown result in solve: %s"
      (Z3.Solver.get_reason_unknown solver)