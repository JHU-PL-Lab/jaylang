open Core
open Path_tree
(* open Dbmc *)
open Dj_common
open Jayil.Ast

module Sudu = From_dbmc.Solver.SuduZ3

(*
  Mutable record that tracks a run through the evaluation. aka "interpreter session"
*)
module Concrete =
  struct
    open From_dbmc

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
        is_debug        : bool
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
      ()
      (* G.add_edge alias_graph x1 x2 *)

    (* Say that x is the variable for the clause body that evaluates to dvalue *)
    let add_val_def_mapping (x : Id_with_stack.t) (vdef : (clause_body * Dvalue.t)) ({ val_def_map; _ } : t) : unit =
      ()
      (* Hashtbl.add_exn ~key:x ~data:vdef val_def_map *)

  end

module Symbolic = Symbolic_session

module Status =
  struct
    type t =
      | In_progress of { pruned : bool }
      | Found_abort of (Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jil_input.t list [@compare.ignore])
      | Exhausted of { pruned : bool }
      [@@deriving compare, sexp]

    let prune (x : t) : t =
      match x with
      | In_progress _ -> In_progress { pruned = true }
      | Exhausted _ -> Exhausted { pruned = true }
      | _ -> x

    let quit (x : t) : bool =
      match x with
      | Found_abort _
      | Type_mismatch _
      | Exhausted _ -> true
      | In_progress _ -> false

    let finish (x : t) : t =
      match x with
      | In_progress { pruned } -> Exhausted { pruned }
      | _ -> x

    let to_string (x : t) : string =
      match x with
      | Found_abort _ -> "Found abort in interpretation"
      | Type_mismatch _ -> "Found type mismatch in interpretation"
      | In_progress { pruned = true } -> "In progress after interpretation (has pruned so far)"
      | In_progress _ -> "In progress after interpretation"
      | Exhausted { pruned = true } -> "Exhausted pruned true"
      | Exhausted _ -> "Exhausted full tree"
  end

type t =
  { tree         : Root.t (* pointer to the root of the entire tree of paths *)
  ; target_queue : Target_queue.t
  ; run_num      : int
  ; options      : Options.t
  ; status       : Status.t
  ; last_sym     : Symbolic.Dead.t option }

let empty : t =
  { tree         = Root.empty
  ; target_queue = Target_queue.empty
  ; run_num      = 0
  ; options      = Options.default
  ; status       = Status.In_progress { pruned = false }
  ; last_sym     = None }

let with_options : (t -> t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with options = r
    ; target_queue = Options.Fun.appl Target_queue.with_options r x.target_queue } 

let accum_symbolic (x : t) (sym : Symbolic.t) : t =
  let dead_sym = Symbolic.finish sym x.tree in
  let new_status =
    match Symbolic.Dead.get_status dead_sym with
    | Symbolic.Status.Found_abort inputs -> Status.Found_abort inputs
    | Type_mismatch inputs -> Type_mismatch inputs
    | Finished_interpretation { pruned = true } -> Status.prune x.status
    | _ -> x.status
  in
  { x with
    tree         = Symbolic.Dead.root dead_sym
  ; target_queue = Target_queue.push_list x.target_queue @@ Symbolic.Dead.targets dead_sym
  ; status       = new_status
  ; last_sym     = Some dead_sym }

let [@landmarks] check_solver solver =
  Z3.Solver.check solver []

let [@landmarks] make_solver () =
  Z3.Solver.mk_solver Sudu.ctx None

(* based on the landmarks, it's taking about as long to make the solver and load it as it is to solve *)
(* This motivates a change to use the internal stack *)
let [@landmarks] load_solver solver formulas =
  Z3.Solver.add solver formulas;
  solver

(* This shows it might be faster to not load any formulas but just run 'check' *)
let[@landmarks] check_solver' formulas =
  let new_solver = Z3.Solver.mk_solver Sudu.ctx None in
  Z3.Solver.check new_solver formulas

let apply_options_symbolic (x : t) (sym : Symbolic.t) : Symbolic.t =
  Options.Fun.appl Symbolic.with_options x.options sym

(* $ OCAML_LANDMARKS=on ./_build/... *)
let[@landmarks] next (x : t) : [ `Done of Status.t | `Next of (t * Symbolic.t * Concrete.t) ] =
  let pop_kind =
    (* match x.last_sym with
    | Some s when Symbolic.Dead.is_reach_max_step s -> Target_queue.Pop_kind.BFS (* only does BFS when last symbolic run reached max step *)
    | _ -> Random *)
    Target_queue.Pop_kind.BFS
  in
  let rec next (x : t) : [ `Done of Status.t | `Next of (t * Symbolic.t * Concrete.t) ] =
    if Status.quit x.status then done_ x else
    match Target_queue.pop ~kind:pop_kind x.target_queue with
    | Some (target, target_queue) -> 
      solve_for_target { x with target_queue } target
    | None when x.run_num = 0 ->
      `Next (
        { x with run_num = 1 }
        , apply_options_symbolic x Symbolic.empty
        , Concrete.create Concolic_feeder.default x.options.global_max_step)
    | None -> done_ x (* no targets left, so done *)

  and solve_for_target (x : t) (target : Target.t) =
    let t0 = Caml_unix.gettimeofday () in
    let new_solver = load_solver (make_solver ()) (Target.to_formulas target x.tree) in
    From_dbmc.Solver.set_timeout_sec Sudu.ctx (Some (Core.Time_float.Span.of_sec x.options.solver_timeout_sec));
    Log.Export.CLog.debug (fun m -> m "Solving for target %s\n" (Branch.Runtime.to_string target.branch));
    Log.Export.CLog.debug (fun m -> m "Solver is:\n%s\n" (Z3.Solver.to_string new_solver));
    (* let[@landmarks] _ = check_solver' (Target.to_formulas target x.tree) in *)
    match check_solver new_solver with
    | Z3.Solver.UNSATISFIABLE ->
      let t1 = Caml_unix.gettimeofday () in
      Log.Export.CLog.info (fun m -> m "FOUND UNSATISFIABLE in %fs\n" (t1 -. t0));
      next { x with tree = Root.set_status x.tree target.branch Unsatisfiable target.path }
    | Z3.Solver.UNKNOWN ->
      Log.Export.CLog.info (fun m -> m "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n");
      next { x with tree = Root.set_status x.tree target.branch Unknown target.path }
    | Z3.Solver.SATISFIABLE ->
      Log.Export.CLog.app (fun m -> m "FOUND SOLUTION FOR BRANCH: %s\n" (Branch.to_string @@ Branch.Runtime.to_ast_branch target.branch));
      `Next (
        { x with run_num = x.run_num + 1 }
        , apply_options_symbolic x @@ Symbolic.make x.tree target
        , Z3.Solver.get_model new_solver
          |> Core.Option.value_exn
          |> Concolic_feeder.from_model
          |> fun feeder -> Concrete.create feeder x.options.global_max_step
      )

  and done_ (x : t) =
    Log.Export.CLog.info (fun m -> m "Done.\n");
    `Done (Status.finish x.status)
    
  in next x

let run_num ({ run_num ; _ } : t) : int =
  run_num