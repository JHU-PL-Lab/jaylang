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
    module Outcome =
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

      end

    type t =
      { branch_solver : Branch_solver.t
      ; cur_target    : Branch.Runtime.t option
      ; new_targets   : Branch.Runtime.t list
      ; outcome       : Outcome.t
      ; hit_branches  : (Branch.Runtime.t * Branch.Status.t) list
      ; inputs        : (Ident.t * Dvalue.t) list }

    let create_default () : t =
      { branch_solver = Branch_solver.empty
      ; cur_target    = None
      ; new_targets   = []
      ; outcome       = { Outcome.empty with hit_target = true } (* hacky: say no target means target is always hit *)
      ; hit_branches  = []
      ; inputs        = [] }

    (*
      Note that we don't add any persistent formulas to the branch solver upon creating because we rely on
      the main Session.t to add them before solving for the target.

      This is because these concolic sessions will be saved along with the target to be solved for later,
      but in the time between saving the target and actually trying to hit it, some persistent formulas may
      be acquired, and it's least-redundant to only add those right before solving (rather than some of them
      upon creation and the remaining upon solving).
    *)
    let create ~(target : Branch.Runtime.t) : t =
      { (create_default ()) with
        cur_target = Some target
      ; outcome = Outcome.empty }

    let add_formula (session : t) (expr : Z3.Expr.expr) : t =
      { session with branch_solver = Branch_solver.add_formula session.branch_solver expr }

    let add_key_eq_val (session : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      { session with branch_solver = Branch_solver.add_key_eq_val session.branch_solver key v }

    let add_alias (session : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      { session with branch_solver = Branch_solver.add_alias session.branch_solver key1 key2 }

    let add_binop (session : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
      { session with branch_solver = Branch_solver.add_binop session.branch_solver key op left right }

    let is_target (branch : Branch.Runtime.t) (target : Branch.Runtime.t option) : bool =
      Option.map target ~f:(fun x -> Branch.Runtime.compare branch x = 0)
      |> Option.value ~default:false

    let found_abort (session : t) : t =
      let branch = Branch_solver.get_cur_parent_exn session.branch_solver in
      { session with
        hit_branches = (branch, Branch.Status.Found_abort) :: session.hit_branches
      ; outcome = { session.outcome with found_abort = true } }

    let reach_max_step (session : t) : t =
      if Branch_solver.is_global session.branch_solver
      then session (* if reaching max step globally, then I really don't know what to do... *)
      else 
        let branch = Branch_solver.get_cur_parent_exn session.branch_solver in
        { session with
          hit_branches = (branch, Branch.Status.Reach_max_step) :: session.hit_branches
        ; outcome = { session.outcome with reach_max_step = true } }

    let enter_branch (session : t) (branch : Branch.Runtime.t) : t =
      (* Format.printf "Hitting: %s: %s\n"
        (let (Jayil.Ast.Ident x) = branch.branch_key.x in x)
        (Branch.Direction.to_string branch.direction); *)
      { session with
        branch_solver = Branch_solver.enter_branch session.branch_solver branch
      ; new_targets   = Branch.Runtime.other_direction branch :: session.new_targets
      ; hit_branches  = (branch, Branch.Status.Hit) :: session.hit_branches
      ; outcome       = if is_target branch session.cur_target then { session.outcome with hit_target = true } else session.outcome }

    let exit_branch (session : t) : t =
      { session with branch_solver = Branch_solver.exit_branch session.branch_solver }

    let add_input (session : t) (x : Ident.t) (v : Dvalue.t) : t =
      let Ident s = x in
      let n =
        match v with
        | Dvalue.Direct (Value_int n) -> n
        | _ -> failwith "non-int input" (* logically impossible *)
      in
      if Printer.print then Format.printf "Feed %d to %s \n" n s;
      { session with inputs = (x, v) :: session.inputs }

    (* TODO: see about modularizing these internal functions that are helpers for Session.t *)
    (* TODO: maybe this shouldn't be here because it is moreso the logic of Session to determine this. *)
    (* TODO: (this one is most important) don't create a persistent formula out of reach max step until it has happend a few times *)
    (* Creates formulas out of all aborted branches such that the abort side should not be satisfied *)
    let to_persistent_formulas (session : t) : Branch_solver.Formula_set.t =
      List.filter_map session.hit_branches ~f:(fun (b, s) ->
        match s with
        | Branch.Status.Found_abort (*| Reach_max_step*) -> 
          (* Handling reach_max_step like this leads to later "unsatisfiable" branches when really they're unreachable due to reach max step. *)
          (* Format.printf "Creating persistent formula for branch %s\n" (Branch.Runtime.to_ast_branch b |> Branch.to_string); *)
          Branch.Runtime.other_direction b
          |> Branch.Runtime.to_expr
          |> Option.return
        | _ -> None
      )
      |> Branch_solver.Formula_set.of_list

  end

(*
  TODO: maybe instead of a map, which is very safe and correct, I can just keep a set of formulas.
    I'd like to prove whether this might not add more information. Because the formulas are written
    in an "implies" fashion, I don't think any formulas will ever be wrong and conflicting to add,
    but is there ever a formula that helps us solve for a target, but that formula is found in a run
    that doesn't hit the either direction of the target branch? Yes. Think of minimal_example.jil
*)
module Solver_map :
  sig
    type t 
    val empty : t
    val add : t -> Branch.Runtime.t -> Concolic.t -> t
    val get_solver : t -> Branch.Runtime.t -> Branch_solver.t
    (** [get_solver solver_map target] is a branch solver that contains all formulas from any
        concolic session that added [target] to the target stack. *)
  end
  =
  struct
    module M = Map.Make (Branch.Runtime)

    (* TODO: could be a little faster to maintain a solver and merge upon adding. Depends on use case I think *)
    (* ^ it also seems from current usage that I can only keep the solver from the session instead of the whole session *)
    type t = Concolic.t list M.t

    let empty = M.empty
    let add (m : t) (target : Branch.Runtime.t) (c : Concolic.t) : t =
      Map.update m target ~f:(function
        | Some ls -> c :: ls 
        | None -> [c]
      )

    let get_solver (m : t) (target : Branch.Runtime.t) : Branch_solver.t =
      match Map.find m target with
      | None -> Branch_solver.empty
      | Some session_list ->
        List.fold
          session_list
          ~init:Branch_solver.empty
          ~f:(fun acc s -> Branch_solver.merge acc s.branch_solver)
  end

module Max_step_counter =
  struct
    module M = Map.Make (Branch)

    type t = int M.t

    (* each branch can hit max step this many times and still not yet be off limits *)
    let num_allowed_trials = 5

    let empty = M.empty

    (* TODO: we're only setting some runtime branch to off limits, but other depths (of recursion) might still be satisfied *)
    (*  ^ We need to never allow ANY depth of recursion hit that branch. This seems like a hard problem. *)
    (*  ^ This also makes me wonder how we should target branches. Should I not mark a branch as unsatisfiable until ALL depths are unsatisfiable? *)
    let inc (m : t) (branch : Branch.Runtime.t) : t * Z3.Expr.expr option =
      let ast_branch = Branch.Runtime.to_ast_branch branch in
      let set x = Map.set m ~key:ast_branch ~data:x in
      match Map.find m ast_branch with
      | None -> set 1, None (* max step has been hit once now *)
      | Some c when c < num_allowed_trials -> set (c + 1), None
      | Some c -> set (c + 1), Some (branch |> Branch.Runtime.other_direction |> Branch.Runtime.to_expr)
  end

type t = 
  { branch_store        : Branch.Status_store.t
  ; persistent_formulas : Branch_solver.Formula_set.t
  ; target_stack        : Branch.Runtime.t list
  ; solver_map          : Solver_map.t (* map targets to all sessions that find them *)
  ; global_max_step     : int
  ; max_step_counter    : Max_step_counter.t
  ; run_num             : int }

let default_global_max_step = Int.(10 ** 3)

let default : t =
  { branch_store        = Branch.Status_store.empty 
  ; persistent_formulas = Branch_solver.Formula_set.empty
  ; target_stack        = []
  ; solver_map          = Solver_map.empty
  ; global_max_step     = default_global_max_step
  ; max_step_counter    = Max_step_counter.empty
  ; run_num             = 0 }

let load_branches (session : t) (expr : Jayil.Ast.expr) : t =
  let branch_store = Branch.Status_store.find_branches expr session.branch_store in
  { session with branch_store }

(* let default_input_feeder : Input_feeder.t =
  fun _ -> Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl (-10) 10) *)

let rec next (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] =
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
    end

let finish (session : t) : t =
  { session with branch_store = Branch.Status_store.finish session.branch_store ; target_stack = [] }

let print ({ branch_store ; target_stack ; _ } : t) : unit =
  Branch.Status_store.print branch_store

let accum_concolic (session : t) (concolic : Concolic.t) : t =
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
  ; persistent_formulas = Branch_solver.Formula_set.union session.persistent_formulas persistent_formulas }

let branch_store (session : t) : Branch.Status_store.t =
  session.branch_store