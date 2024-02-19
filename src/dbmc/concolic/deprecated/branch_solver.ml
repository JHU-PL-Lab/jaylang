(* 
(* mutates solver to add abort formulas if satisfiable *)
let solve_abort 
  (solver : Z3.Solver.solver)
  (formula_tracker : Formula_tracker.t)
  (branch_tracker : Branch_tracker.t)
  : [ `Continue | `Quit of Branch_tracker.Status.t ]
  (* Continue solving because satisfiable, or quit because of some new status for target *)
  =
  let abort_formulas =
    Formula_tracker.abort_formulas formula_tracker
    @@ Branch_tracker.get_aborts branch_tracker
  in
  Z3.Solver.add solver abort_formulas;
  match Z3.Solver.check solver [] with
  | Z3.Solver.UNSATISFIABLE -> `Quit Branch_tracker.Status.Unreachable_because_abort
  | Z3.Solver.UNKNOWN -> `Quit (Branch_tracker.Status.Unknown 1)
  | Z3.Solver.SATISFIABLE -> `Continue

(* does not mutate solver, but expects that the model is gotten after this *)
let solve_max_step
  (solver : Z3.Solver.solver)
  (formula_tracker : Formula_tracker.t)
  (branch_tracker : Branch_tracker.t)
  : [ `Continue | `Quit of Branch_tracker.Status.t ]
  =
  let max_step_formulas =
    Formula_tracker.max_step_formulas formula_tracker
    @@ Branch_tracker.get_aborts branch_tracker
  in
  match max_step_formulas with 
  | [] -> `Continue
  | _ -> begin
    match Z3.Solver.check solver max_step_formulas with
    | Z3.Solver.UNSATISFIABLE -> `Quit Branch_tracker.Status.Unreachable_because_max_step
    | Z3.Solver.UNKNOWN -> `Quit (Branch_tracker.Status.Unknown 1)
    | Z3.Solver.SATISFIABLE -> `Continue
  end


let check_solver
  (target : Branch.t)
  (formula_tracker : Formula_tracker.t)
  (branch_tracker : Branch_tracker.t)
  : [ `Unsolvable of Branch_tracker.Status.t | `Solved of Z3.Model.model ]
  =
  let formulas =
    Formula_tracker.all_formulas
      formula_tracker
      ~target
      ~aborts:[]
      ~max_steps:[]
      ~allow_repeat_inputs:true
  in
  let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
  Z3.Solver.add new_solver formulas;
  (* First solve for target without abort or max step formulas *)
  (* Format.printf "%s\n" (Z3.Solver.to_string new_solver); *)
  match Z3.Solver.check new_solver [] with
  | Z3.Solver.UNSATISFIABLE -> Format.printf "FOUND UNSATISFIABLE\n"; `Unsolvable Branch_tracker.Status.Unsatisfiable
  | Z3.Solver.UNKNOWN -> `Unsolvable (Branch_tracker.Status.Unknown 1)
  | Z3.Solver.SATISFIABLE -> begin
    Z3.Solver.add new_solver [ Formula_tracker.input_formula ]; (* now disallow repeat inputs *)
    match solve_abort new_solver formula_tracker branch_tracker with
    | `Quit status -> `Unsolvable status
    | `Continue -> begin
      match solve_max_step new_solver formula_tracker branch_tracker with
      | `Quit status -> `Unsolvable status
      | `Continue -> `Solved (Z3.Solver.get_model new_solver |> Core.Option.value_exn)
    end
  end *)

let check_solver
  (target : Branch.t)
  (formula_tracker : Formula_tracker.t)
  (branch_tracker : Branch_tracker.t)
  : [ `Unsolvable | `Timeout | `Solved of Z3.Model.model ]
  =
  let formulas =
    Formula_tracker.all_formulas
      formula_tracker
      ~target
      ~aborts:(Branch_tracker.get_aborts branch_tracker)
      ~max_steps:(Branch_tracker.get_max_steps branch_tracker)
      ~allow_repeat_inputs:false
  in
  let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
  Z3.Solver.add new_solver formulas;
  (* Format.printf "%s\n" (Z3.Solver.to_string new_solver); *)
  match Z3.Solver.check new_solver [] with
  | Z3.Solver.UNSATISFIABLE -> Format.printf "FOUND UNSATISFIABLE\n"; `Unsolvable
  | Z3.Solver.UNKNOWN -> Format.printf "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n"; `Timeout
  | Z3.Solver.SATISFIABLE -> `Solved (Z3.Solver.get_model new_solver |> Core.Option.value_exn)