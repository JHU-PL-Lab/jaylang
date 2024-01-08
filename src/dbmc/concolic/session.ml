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
  Mutable record that tracks a run through the evaluation.   
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
    (* module Outcome =
      struct
        type t =
          { found_abort    : bool
          ; hit_target     : bool
          ; reach_max_step : bool }
          [@@deriving compare, sexp]

        let empty : t =
          { found_abort    = false
          ; hit_target     = false
          ; reach_max_step = false }

      end *)

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

    (* let is_target (branch : Branch.Runtime.t) (target : Branch.Runtime.t option) : bool =
      Option.map target ~f:(fun x -> Branch.Runtime.compare branch x = 0)
      |> Option.value ~default:false *)

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
      { session with input = (key, n) :: session.input }
  end

type t = 
  { branch_tracker  : Branch_tracker.t
  ; formula_tracker : Formula_tracker.t
  ; global_max_step : int
  ; run_num         : int}

let default_global_max_step = Int.(10 ** 3)

let default : t =
  { branch_tracker  = Branch_tracker.empty
  ; formula_tracker = Formula_tracker.empty
  ; global_max_step = default_global_max_step
  ; run_num         = 0 }

let of_expr (expr : Jayil.Ast.expr) : t =
  { default with branch_tracker = Branch_tracker.of_expr expr }

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
  (* TODO: logic for statuses wrt aborts and max steps *)
  let formulas =
    Formula_tracker.all_formulas
      session.formula_tracker
      ~target
      ~aborts:(Branch_tracker.get_aborts session.branch_tracker)
      ~max_steps:(Branch_tracker.get_max_steps session.branch_tracker)
  in
  let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
  Z3.Solver.add new_solver formulas;
  Z3.Solver.check new_solver [] (* `formulas` contains all necessary formulas, and there are no additional ones to check *)
  |> Solver.SuduZ3.get_model new_solver
  |> function
    | None -> (* not solvable *)
      next { session with branch_tracker = Branch_tracker.set_unsatisfiable session.branch_tracker target }
    | Some model -> (* solvable with model *)
      `Next ({ session with run_num = session.run_num + 1 }
            , Concolic.create ~target ~formula_tracker:session.formula_tracker
            , Eval.create (Concolic_feeder.from_model model) session.global_max_step )


(* let rec next (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] =
  let is_skip target =
    (* Don't try to re-solve for anything that is hit, missed, unsatisfiable, or contains an abort *)
    Branch.Status_store.get_status session.branch_store
    @@ Branch.Runtime.to_ast_branch target
    |> function
      | Branch.Status.Hit | Unsatisfiable | Found_abort -> true
      | Unhit | Missed | Reach_max_step | Unreachable -> false
  in
  match Branch.Status_store.get_unhit_branch session.branch_store, session.target_stack with
  | None, _ -> (* all branches have been considered *)
    `Done session
  | _, [] when session.run_num > 0 -> (* no targets have been found that we can target next *)
    `Done session
  | _, [] -> (* no targets, but this is the first run, so use the default *)
    `Next ( { session with run_num = session.run_num + 1 }
          , Concolic.create_default ()
          , Eval.create Concolic_feeder.default session.global_max_step )
  | _, target :: tl when is_skip target -> (* next target should not be considered *)
    (* Format.printf "Skipping already-hit target %s\n" (Branch.Runtime.to_string target); *)
    next { session with target_stack = tl }
  | _, target :: tl -> begin (* next target is unhit, so we'll solve for it *)
    (* add all persistent formulas before trying to solve *)
    target
    |> Solver_map.get_solver session.solver_map
    |> Branch_solver.add_formula_set session.persistent_formulas
    |> Fn.flip Branch_solver.get_feeder target
    |> function (* do other people think this is bad style? *)
      | Ok input_feeder ->
        `Next ({ session with target_stack = tl ; run_num = session.run_num + 1 }
              , Concolic.create ~target
              , Eval.create input_feeder session.global_max_step )
      | Error b ->
        (* Format.printf "Unsatisfiable branch %s. Continuing to next target.\n" (Branch.to_string b); *)
        let branch_store = 
          Branch.Status_store.set_branch_status
            ~new_status:Branch.Status.Unsatisfiable
            session.branch_store
            b
        in
        next { session with target_stack = tl ; branch_store }
    end *)

let finish (session : t) : t =
  { session with branch_tracker = Branch_tracker.finish session.branch_tracker }

let print ({ branch_tracker ; _ } : t) : unit =
  Branch_tracker.print branch_tracker

let accum_concolic (session : t) (concolic : Concolic.t) : t =
  { session with
    formula_tracker = concolic.formula_tracker (* completely overwrite because we passed it in earlier to make the concolic session *)
  ; branch_tracker = Branch_tracker.collect_runtime session.branch_tracker concolic.branch_tracker concolic.input }

(* let accum_concolic (session : t) (concolic : Concolic.t) : t =
  let branch_store =
    concolic.hit_branches
    |> List.rev (* need to consider branches in the order they were hit *)
    |> List.fold ~init:session.branch_store ~f:(fun acc (branch, new_status) ->
      Branch.Status_store.set_branch_status ~new_status acc
      @@ Branch.Runtime.to_ast_branch branch
      )
    |> function
      | branch_store when concolic.outcome.hit_target || concolic.outcome.found_abort -> branch_store
      | branch_store -> (* missed target and no abort (if missed and had abort, the abort could have been the reason, so try again) *)
        Branch.Status_store.set_branch_status ~new_status:Branch.Status.Missed branch_store
        @@ Branch.Runtime.to_ast_branch (Option.value_exn concolic.cur_target)
  in
  let persistent_formulas = Concolic.to_persistent_formulas concolic in
  let target_stack, solver_map = 
    let map_targets = List.map ~f:(fun target -> (target, concolic)) in
    match concolic.outcome.hit_target, concolic.outcome.found_abort with
    | false, _ -> begin (* used to be false, true *)
      (* Format.printf "Did not make meaningful progress, so discarding new targets\n"; *)
      (* Put the original target back on the stack to try to hit it again, but the rest are not new targets *)
      match concolic.cur_target with
      | Some target -> target :: session.target_stack, Solver_map.add session.solver_map target concolic
      | None -> session.target_stack, session.solver_map
      end
    | true, _ ->
      (* has meaningful targets, so prioritize them *)
      concolic.new_targets @ session.target_stack
      , List.fold concolic.new_targets ~init:session.solver_map ~f:(fun m target -> Solver_map.add m target concolic)
    (* | false, _ -> session.target_stack, session.solver_map TODO: optionally handle targets from missed target, e.g. if missed because abort, try again  *)
    (* TODO: once the solver hasn't changed between runs (which might be difficult to manage if overadding formulas), then stop trying for missed targets. *)
    (* TODO: if missed again, put at back of stack to gain even more information first. Need sofisticated tracking of what led to misses, hits, etc, and make sure not trying to solve for same miss *)
      (* ^ I only need this if solver logic is wrong. If we continue to miss, then we're hitting unknown branches and gaining information to eventually hit or call unsatisfiable. *)
  in
  let max_step_counter, formula_opt =
    if not concolic.outcome.reach_max_step
    then session.max_step_counter, None
    else
      List.find_map concolic.hit_branches ~f:(
        function
        | (branch, Branch.Status.Reach_max_step) -> Some (Max_step_counter.inc session.max_step_counter branch)
        | _ -> None
      )
      |> Option.value_exn (* safe because of the encompassing if statement *)
  in
  let persistent_formulas = Branch_solver.Formula_set.union persistent_formulas (formula_opt |> Option.to_list |> Branch_solver.Formula_set.of_list)
  in
  { session with
    branch_store 
  ; target_stack
  ; solver_map
  ; persistent_formulas = Branch_solver.Formula_set.union session.persistent_formulas persistent_formulas } *)

let branch_tracker ({ branch_tracker ; _ } : t) : Branch_tracker.t =
  branch_tracker