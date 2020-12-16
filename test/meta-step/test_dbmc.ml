open Batteries
open Program_samples

open Odefa_symbolic_interpreter

let ctx = Z3.mk_context []

module Z3API = Dmbc_solver.Solver.Make (struct let ctx = ctx end) ()

let test program target =
  let solver  = Z3.Solver.mk_solver ctx None in
  let phis = Odefa_symbolic_interpreter.Dbmc.lookup_main program target in
  print_endline @@ Constraint.list_to_string phis;
  let z3_phis = List.map Z3API.z3_phis_of_smt_phi phis in
  Z3.Solver.add solver z3_phis;
  print_endline @@ Z3.Solver.to_string solver;
  match Z3API.check_and_get_model solver with
  | Some model ->
    let _ : Relative_stack.t = Z3API.get_top_stack model in
    ()
  | None -> ()

let () =
  (* test ep1 "x";
     test ep2 "y";
     test ep3 "y"; *)
  test ep4 "y";
  (* test ep5 "y";
      test ep6 "y"; 
        test ep7 "r";
        test ep8 "target";
        test ep9 "target";
        test ep10 "target"; *)

  ()