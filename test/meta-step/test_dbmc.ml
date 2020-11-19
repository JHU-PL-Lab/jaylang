open Batteries
open Program_samples

open Odefa_symbolic_interpreter

let ctx = Z3.mk_context []

module Z3API = Odefa_solver.Solver.Make (struct let ctx = ctx end) ()

let test program target =
  (* let open Odefa_solver in *)
  let solver  = Z3.Solver.mk_solver ctx None in
  let phis = Dbmc.lookup_main program (Ident target) in
  let smt_phis = List.map Dbmc.SMT_Phi.smt_phi_of_phi phis in
  print_endline @@ Printf.sprintf "%s"
    (Jhupllib.Pp_utils.pp_to_string
       (Jhupllib.Pp_utils.pp_list Constraint.pp) smt_phis);
  let z3_phis = List.map Z3API.z3_phis_of_smt_phi smt_phis in
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

(* let test2 program target = 
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
    print_endline "ERROR" *)

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
  test ep10 "target";

  ()