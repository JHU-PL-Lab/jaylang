open Core
open Path_tree
open Dj_common
open Jayil.Ast

(*
  Mutable record that tracks a run through the evaluation. aka "interpreter session"
*)
module Concrete =
  struct
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

module Symbolic = Symbolic_session

type t =
  { tree         : Root.t (* pointer to the root of the entire tree of paths *)
  ; target_queue : Target_queue.t
  ; branch_info  : Branch_info.t
  ; run_num      : int
  ; options      : Concolic_options.t
  ; quit         : bool
  ; has_pruned   : bool (* true iff some evaluation hit more nodes than are allowed to be kept *)
  ; last_sym     : Symbolic.t option }

let empty : t =
  { tree         = Root.empty
  ; target_queue = Target_queue.empty
  ; branch_info  = Branch_info.empty
  ; run_num      = 0
  ; options      = Concolic_options.default
  ; quit         = false
  ; has_pruned   = false
  ; last_sym     = None }

let with_options : (t -> t) Concolic_options.Fun.t =
  Concolic_options.Fun.make
  @@ fun (r : Concolic_options.t) -> fun (x : t) -> { x with options = r } 

let of_expr (expr : Jayil.Ast.expr) : t =
  { empty with branch_info = Branch_info.of_expr expr }

let accum_symbolic (x : t) (sym : Symbolic.t) : t =
  let sym = Symbolic.finish sym x.tree in
  { x with
    tree         = Symbolic.root_exn sym
  ; has_pruned   = x.has_pruned || Symbolic.hit_max_depth sym
  ; branch_info  = Branch_info.merge x.branch_info @@ Symbolic.branch_info sym
  ; target_queue = Target_queue.push_list x.target_queue @@ Symbolic.targets_exn sym
  ; quit         = x.quit || x.options.quit_on_abort && Branch_info.contains (Symbolic.branch_info sym) Found_abort
  ; last_sym     = Some sym }

let [@landmarks] check_solver solver =
  Z3.Solver.check solver []

let [@landmarks] make_solver () =
  Z3.Solver.mk_solver Solver.SuduZ3.ctx None

(* based on the landmarks, it's taking about as long to make the solver and load it as it is to solve *)
(* This motivates a change to use the internal stack *)
let [@landmarks] load_solver solver formulas =
  Z3.Solver.add solver formulas;
  solver

(* This shows it might be faster to not load any formulas but just run 'check' *)
let[@landmarks] check_solver' formulas =
  let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
  Z3.Solver.check new_solver formulas

let apply_options_symbolic (x : t) (sym : Symbolic.t) : Symbolic.t =
  Concolic_options.Fun.appl Symbolic.with_options x.options sym

(* $ OCAML_LANDMARKS=on ./_build/... *)
(* `Done (branch_info, has_pruned) *)
let[@landmarks] next (x : t) : [ `Done of (Branch_info.t * bool) | `Next of (t * Symbolic.t * Concrete.t) ] =
  if x.quit then
    (Format.printf "Done. Tree size is %d\n" (Root.size x.tree);
    `Done (x.branch_info, x.has_pruned))
  else
  let pop_kind =
    match x.last_sym with
    | Some s when Symbolic.is_reach_max_step s -> `BFS (* only does BFS when last symbolic run reached max step *)
    | _ -> `DFS
  in
  let rec next (x : t) : [ `Done of (Branch_info.t * bool) | `Next of (t * Symbolic.t * Concrete.t) ] =
    (* It's never realistically relevant to quit when all branches are hit because at least one will have an abort *)
    (* if Branch_tracker.Status_store.Without_payload.all_hit x.branches then `Done x.branches else *)
    match Target_queue.pop ~kind:pop_kind x.target_queue with
    | Some (target, target_queue) -> 
      solve_for_target { x with target_queue } target
    | None when x.run_num = 0 ->
      `Next (
        { x with run_num = 1 }
        , apply_options_symbolic x Symbolic.empty
        , Concrete.create Concolic_feeder.default x.options.global_max_step)
    | None -> (* no targets left, so done *)
      Format.printf "Done. Tree size is %d\n" (Root.size x.tree);
      `Done (x.branch_info, x.has_pruned)

  and solve_for_target (x : t) (target : Target.t) =
    let t0 = Caml_unix.gettimeofday () in
    let new_solver = load_solver (make_solver ()) (Target.to_formulas target x.tree) in
    Solver.set_timeout_sec Solver.SuduZ3.ctx (Some (Core.Time_float.Span.of_sec x.options.solver_timeout_sec));
    if x.options.print_solver then
      begin
      Format.printf "Solving for target %s\n" (Branch.Runtime.to_string target.child.branch);
      Format.printf "Solver is:\n%s\n" (Z3.Solver.to_string new_solver);
      end;
    (* let[@landmarks] _ = check_solver' (Target.to_formulas target x.tree) in *)
    match check_solver new_solver with
    | Z3.Solver.UNSATISFIABLE ->
      let t1 = Caml_unix.gettimeofday () in
      Format.printf "FOUND UNSATISFIABLE in %fs\n" (t1 -. t0); (* TODO: add formula that says it's not satisfiable so less solving is necessary *)
      next { x with tree = Root.set_status x.tree target.child Status.Unsatisfiable target.path }
    | Z3.Solver.UNKNOWN ->
      Format.printf "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n";
      next { x with tree = Root.set_status x.tree target.child Status.Unknown target.path }
    | Z3.Solver.SATISFIABLE ->
      Format.printf "FOUND SOLUTION FOR BRANCH: %s\n" (Branch.to_string @@ Branch.Runtime.to_ast_branch target.child.branch);
      `Next (
        { x with run_num = x.run_num + 1 }
        , apply_options_symbolic x @@ Symbolic.make x.tree target
        , Z3.Solver.get_model new_solver
          |> Core.Option.value_exn
          |> Concolic_feeder.from_model
          |> fun feeder -> Concrete.create feeder x.options.global_max_step
      )
    
  in next x

let branch_info ({ branch_info ; _ } : t) : Branch_info.t =
  branch_info

let run_num ({ run_num ; _ } : t) : int =
  run_num