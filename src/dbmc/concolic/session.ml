open Core
open Dj_common
open Jayil.Ast

module Mode =
  struct
    type t =
      | Plain
      | With_target_x of Id.t
      | With_full_target of Id.t * Concrete_stack.t

    module Debug =
      struct
        type t = No_debug | Debug_clause of (Id.t -> Concrete_stack.t -> value -> unit)
      end
  end

module G = Graph.Imperative.Digraph.ConcreteBidirectional (Id_with_stack)

(*
  Mutable record that tracks a run through the evaluation. aka "interpreter session"
*)
module Eval =
  struct
    type t =
      { (* mode *)
        input_feeder    : Input_feeder.t
      ; mode            : Mode.t
      ; (* tuning *)
        step            : int ref
      ; max_step        : int option
      ; (* book-keeping*)
        alias_graph     : G.t
      ; (* debug *)
        is_debug        : bool (* TODO: get rid of this *) (* can use Mode.Debug.t instead *)
      ; debug_mode      : Mode.Debug.t
      ; val_def_map     : (Id_with_stack.t, clause_body * Dvalue.t) Hashtbl.t
      ; term_detail_map : (Lookup_key.t, Lookup_detail.t) Hashtbl.t
      ; block_map       : Cfg.block Jayil.Ast.Ident_map.t
      ; rstk_picked     : (Rstack.t, bool) Hashtbl.t
      ; lookup_alert    : Lookup_key.t Hash_set.t } 

    let create_default () =
      { input_feeder    = Fn.const 42
      ; mode            = Plain
      ; max_step        = None
      ; is_debug        = false
      ; debug_mode      = No_debug
      ; step            = ref 0
      ; alias_graph     = G.create ()
      ; val_def_map     = Hashtbl.create (module Id_with_stack)
      ; block_map       = Jayil.Ast.Ident_map.empty
      ; term_detail_map = Hashtbl.create (module Lookup_key)
      ; rstk_picked     = Hashtbl.create (module Rstack)
      ; lookup_alert    = Hash_set.create (module Lookup_key) }

    (* Most fields in global_state are hash tables or hash sets *)
    (* Not needed currently *)
    (* let create
      ?max_step
      ?(debug_mode = Mode.Debug.No_debug)
      (state        : Global_state.t)
      (config       : Global_config.t)
      (mode         : Mode.t)
      (input_feeder : Input_feeder.t)
      : t
      =
      { input_feeder
      ; mode
      ; max_step
      ; is_debug        = config.debug_interpreter
      ; debug_mode
      ; step            = ref 0
      ; alias_graph     = G.create()
      ; block_map       = state.block_map
      ; val_def_map     = Hashtbl.create (module Id_with_stack)
      ; term_detail_map = state.term_detail_map
      ; rstk_picked     = state.rstk_picked
      ; lookup_alert    = state.lookup_alert } *)

    let create (input_feeder : Input_feeder.t) (global_max_step : int) : t =
      { (create_default ()) with 
        input_feeder
      ; max_step = Some global_max_step }

    (* Say that x1 is an alias for x2. x1 is defined *after* x2 and points to x2. *)
    let add_alias (x1 : Id_with_stack.t) (x2 : Id_with_stack.t) ({ alias_graph; _ } : t) : unit =
      G.add_edge alias_graph x1 x2

    (* Say that x is the variable for the clause body that evaluates to dvalue *)
    let add_val_def_mapping (x : Id_with_stack.t) (vdef : (clause_body * Dvalue.t)) ({ val_def_map; _ } : t) : unit =
      Hashtbl.add_exn ~key:x ~data:vdef val_def_map

  end

module Concolic =
  struct

    type t =
      { formula_tracker : Formula_tracker.t
      ; branch_tracker  : Branch_tracker.Runtime.t
      ; input           : Branch_tracker.Input.t }

    let default : t =
      { formula_tracker = Formula_tracker.empty
      ; branch_tracker  = Branch_tracker.Runtime.empty
      ; input           = [] }

    let create ~(target : Branch.t) ~(formula_tracker : Formula_tracker.t) : t =
      { default with branch_tracker = Branch_tracker.Runtime.with_target target ; formula_tracker }

    let add_formula (session : t) (expr : Z3.Expr.expr) : t =
      { session with formula_tracker = Formula_tracker.add_formula session.formula_tracker expr }

    let add_key_eq_val (session : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      { session with formula_tracker = Formula_tracker.add_key_eq_val session.formula_tracker key v }

    let add_alias (session : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      { session with formula_tracker = Formula_tracker.add_alias session.formula_tracker key1 key2 }

    let add_binop (session : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
      { session with formula_tracker = Formula_tracker.add_binop session.formula_tracker key op left right }

    let found_abort (session : t) : t =
      { session with
        formula_tracker = Formula_tracker.exit_until_global session.formula_tracker
      ; branch_tracker = Branch_tracker.Runtime.found_abort session.branch_tracker }

    let reach_max_step (session : t) : t =
      { session with
        formula_tracker = Formula_tracker.exit_until_global session.formula_tracker
      ; branch_tracker = Branch_tracker.Runtime.reach_max_step session.branch_tracker }

    let enter_branch (session : t) (branch : Branch.Runtime.t) : t =
      (* Format.printf "Hitting: %s: %s\n"
        (let (Jayil.Ast.Ident x) = branch.branch_key.x in x)
        (Branch.Direction.to_string branch.direction); *)
      { session with
        formula_tracker = Formula_tracker.enter_branch session.formula_tracker branch
      ; branch_tracker = Branch_tracker.Runtime.hit_branch session.branch_tracker (Branch.Runtime.to_ast_branch branch) }

    let exit_branch (session : t) : t =
      { session with
        formula_tracker = Formula_tracker.exit_branch session.formula_tracker
      ; branch_tracker = Branch_tracker.Runtime.exit_branch session.branch_tracker }

    let add_input (session : t) (key : Lookup_key.t) (v : Dvalue.t) : t =
      let Ident s = key.x in
      let n =
        match v with
        | Dvalue.Direct (Value_int n) -> n
        | _ -> failwith "non-int input" (* logically impossible *)
      in
      if Printer.print then Format.printf "Feed %d to %s \n" n s;
      { session with input = (key, n) :: session.input
      ; formula_tracker = Formula_tracker.add_input session.formula_tracker key (Value_int n) }
  end

type t = 
  { branch_tracker   : Branch_tracker.t
  ; formula_tracker  : Formula_tracker.t
  ; solver_timeout_s : float (* seconds allowed for each solve *)
  ; global_max_step  : int
  ; run_num          : int}

let default_global_max_step = Int.(5 * 10 ** 2)

let default_solver_timeout_s = 0.25

let default : t =
  { branch_tracker   = Branch_tracker.empty
  ; formula_tracker  = Formula_tracker.empty
  ; solver_timeout_s = default_solver_timeout_s
  ; global_max_step  = default_global_max_step
  ; run_num          = 0 }

let of_expr (expr : Jayil.Ast.expr) : t =
  { default with branch_tracker = Branch_tracker.of_expr expr }

(* TODO: handle max step on first run leading to not hitting later branches, so they appear unreachable when
   really they're not. This happens because solver times out on runs containing so many formulas, and then
   it can't even run a second time, so the branches aren't found. *)
let rec next (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] =
  match Branch_tracker.next_target session.branch_tracker with
  | None, branch_tracker when session.run_num > 0 -> `Done { session with branch_tracker }
  | None, branch_tracker -> (* no targets, but this is the first run, so use the default *)
    `Next ({ session with run_num = session.run_num + 1 ; branch_tracker }
          , Concolic.default
          , Eval.create Concolic_feeder.default session.global_max_step )
  | Some target, branch_tracker ->
    solve_for_target target { session with branch_tracker }

and solve_for_target (target : Branch.t) (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] =
  Solver.set_timeout_sec Solver.SuduZ3.ctx (Some (Core.Time_float.Span.of_sec session.solver_timeout_s));
  Format.printf "Solving for %s\n" (Branch.to_string target);
  let formulas =
    Formula_tracker.all_formulas
      session.formula_tracker
      ~target
      ~aborts:[]
      ~max_steps:[]
  in
  match check_solver session formulas with
  | `Unsolvable status ->
    next { session with branch_tracker = Branch_tracker.set_status session.branch_tracker target status }
  | `Solved model ->
    `Next ({ session with run_num = session.run_num + 1 }
          , Concolic.create ~target ~formula_tracker:session.formula_tracker
          , Eval.create (Concolic_feeder.from_model model) session.global_max_step )

and check_solver
  (session : t)
  (formulas : Z3.Expr.expr list)
  : [ `Unsolvable of Branch_tracker.Status.t | `Solved of Z3.Model.model ]
  =
  let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
  Z3.Solver.add new_solver formulas; (* could be faster to maintain a solver that only ever includes these non-abort and non-max-step formulas *)
  let solve_abort () =
    let abort_formulas =
      Formula_tracker.abort_formulas session.formula_tracker
      @@ Branch_tracker.get_aborts session.branch_tracker
    in
    match abort_formulas with
    | [] -> `Continue []
    | _ -> begin
      match Z3.Solver.check new_solver abort_formulas with
      | Z3.Solver.UNSATISFIABLE -> `Quit Branch_tracker.Status.Unreachable_because_abort
      | Z3.Solver.UNKNOWN -> `Quit Branch_tracker.Status.Unknown
      | Z3.Solver.SATISFIABLE -> `Continue abort_formulas
    end
  in
  let solve_max_step abort_formulas =
    let max_step_formulas =
      Formula_tracker.max_step_formulas session.formula_tracker
      @@ Branch_tracker.get_aborts session.branch_tracker
    in
    match max_step_formulas with 
    | [] -> `Continue
    | _ -> begin
      match Z3.Solver.check new_solver (max_step_formulas @ abort_formulas) with
      | Z3.Solver.UNSATISFIABLE -> `Quit Branch_tracker.Status.Unreachable_because_max_step
      | Z3.Solver.UNKNOWN -> `Quit Branch_tracker.Status.Unknown
      | Z3.Solver.SATISFIABLE -> `Continue
    end
  in
  (* First solve for target without abort or max step formulas *)
  print_endline (Z3.Solver.to_string new_solver);
  match Z3.Solver.check new_solver [] with
  | Z3.Solver.UNSATISFIABLE -> Format.printf "FOUND UNSATISFIABLE\n"; `Unsolvable Branch_tracker.Status.Unsatisfiable
  | Z3.Solver.UNKNOWN -> `Unsolvable Branch_tracker.Status.Unknown
  | Z3.Solver.SATISFIABLE -> begin
    match solve_abort () with
    | `Quit status -> `Unsolvable status
    | `Continue abort_formulas -> begin
      match solve_max_step abort_formulas with
      | `Quit status -> `Unsolvable status
      | `Continue -> `Solved (Z3.Solver.get_model new_solver |> Option.value_exn )
    end
  end

let finish (session : t) : t =
  { session with branch_tracker = Branch_tracker.finish session.branch_tracker }

let print ({ branch_tracker ; _ } : t) : unit =
  Branch_tracker.print branch_tracker

let accum_concolic (session : t) (concolic : Concolic.t) : t =
  { session with
    formula_tracker = concolic.formula_tracker (* completely overwrite because we passed it in earlier to make the concolic session *)
  ; branch_tracker = Branch_tracker.collect_runtime session.branch_tracker concolic.branch_tracker concolic.input }

let branch_tracker ({ branch_tracker ; _ } : t) : Branch_tracker.t =
  branch_tracker