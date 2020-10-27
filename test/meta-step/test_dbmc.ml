open Batteries
open Program_samples

open Odefa_symbolic_interpreter

let test program target = 
  let phis = Dbmc.lookup_main program (Ident target) in
  let smt_phis = List.map Dbmc.SMT_Phi.smt_phi_of_phi phis in
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list Constraint.pp) smt_phis);
  try
    let s = List.fold_left (fun s c -> Solver.add c s) Solver.empty smt_phis in
    match Solver.solve s with
    | _ -> print_endline "SAT"
  with
  | _ ->
    print_endline "UNSAT"

let () =
  test ep1 "x";
  test ep2 "y";
  test ep3 "y";
  test ep4 "y";
  test ep5 "y";
  test ep6 "y"; 
  test ep7 "r";
  test ep8 "target";
  test ep9 "target";
  ()