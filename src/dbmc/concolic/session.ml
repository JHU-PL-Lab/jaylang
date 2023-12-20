open Core
open Dj_common
open Jayil.Ast
(* open Concolic_exceptions *)

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

(*
    If a record has a field that itself has mutable fields, those inner fields can change
    and the outer record does not have to have explicitly mutable fields.
    e.g. type m = { x : int ref }
    type r = { y : m }
    let z = { y = { x = ref 0 }}
    let zy = z.y
    zy.x := 1
    Now z is { y = { x = ref 1 }}, even though z has no ref cells 

  TODO: because I end up splitting them up anyways, maybe I want to not nest them
    and instead I pass them around separately. I just need to think about the `next` implications.
    They are currently coupled because of how the concolic session affects the eval session's feeder.

  TODO: there are parts of the concolic session that don't persist over runs, and some that do.
    I think I should have a Session.t that is for the entire thing, Eval.t is for runtime stuff, Concolic.t
    is for a single run's concolic stuff. Both are mutable.
*)
(* module Concolic =
  struct

    module Permanent_formulas =
      struct
        module Eset = Set.Make
          (struct
            include Z3.Expr
            type t = Z3.Expr.expr
            let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
            let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
          end)
        type t = Eset.t

        let add set x = Set.union set @@ Eset.singleton x
        let join = Set.union
        let fold = Set.fold
        let empty = Eset.empty;
      end

    (*
      The concolic session remembers the branches and formulas. These are additional
      fields that are not needed during the regular evaluation.

      The concolic session actually wraps the eval session, which is mutable
    *)
    type t =
      { branch_store       : Branch.Status_store.t
      ; formula_store      : Branch_solver.t
      ; permanent_formulas : Permanent_formulas.t (* persists between sessions *)
      ; target_stack       : Branch.Runtime.t list
      ; prev_sessions      : t list (* TODO: is this even needed for reach max step? If so, can be an option *)
      ; global_max_step    : int
      ; run_num            : int 
      ; eval               : Eval.t } 

    let load_branches (session : t) (e : expr) : t =
      { session with branch_store = Branch.Status_store.find_branches e session.branch_store }

    let default_global_max_step = Int.(10 ** 3)

    (*
      Default has no target branch, so input will be random between -10 and 10.
    *)
    let default_input_feeder : Input_feeder.t =
      fun _ -> Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl (-10) 10)

    (*
      Concolic eval runs have a max step and specific input feeder to try to reach the target.
    *)
    let create_eval (input_feeder : Input_feeder.t) (global_max_step : int) : Eval.t =
      { (Eval.create_default ()) with 
        input_feeder
      ; max_step = Some global_max_step }

    (*
      To be called before the very first concolic run to get empty tracking variables.   
    *)
    let create_default () =
      { branch_store       = Branch.Status_store.empty
      ; formula_store      = Branch_solver.empty 
      ; permanent_formulas = Permanent_formulas.empty
      ; target_stack       = []
      ; prev_sessions      = []
      ; global_max_step    = default_global_max_step
      ; run_num            = 0 
      ; eval               = create_eval default_input_feeder default_global_max_step }

    let reset_formulas (session : t) : t =
      { session with formula_store = Branch_solver.empty }

    (* TODO: think about if we need to revert more than once *)
    let revert
      (session : t)
      (reason : [ `Abort_before_target | `Max_step_before_target of Branch.Runtime.t ])
      : t option
      =
      let prev_session = List.hd session.prev_sessions in
      let revert prev_session =
        match reason with
        | `Max_step_before_target _ -> failwith "unimplemented revert on max step"
        | `Abort_before_target ->
          (* Didn't hit target at all. For now, discard targets. TODO: see if we need to keep any *)
          (* *)
          { prev_session with
            permanent_formulas = Permanent_formulas.join session.permanent_formulas prev_session.permanent_formulas  
          ; branch_store = session.branch_store (* `next` only adds more information, so we never lose info by overwriting old with new *)
          }
      in
      Option.map ~f:revert
      @@ List.hd session.prev_sessions

    (*
      The next session
      * knocks off any hit or unsatisfiable targets (and marks them as such in the branch store)
      * keeps other items in the branch store
      * increments the run number
      * resets the formula store (so that it can be filled during the next run)
      * creates a brand new eval session
      * uses the top target to determine the input feeder for the eval session

      If there is no possible next target, then returns the branch store.

      TODO: when hitting max step or abort, need to potentially revert to previous session (if the
        current formulas can't make sense) and add all acquired targets to the old stack.
        Because we can potentially enter new branches in an evaluation that fails, we still need
        to add those other branches to the target list or else they'll be deemed unreachable, which
        is not necessarily true.
    *)
    let next (session : t) : [ `Next of t | `Done of Branch.Status_store.t ] =
      let with_input_feeder (session : t) (input_feeder : Input_feeder.t) : t =
        let new_eval_session = create_eval input_feeder session.global_max_step in
        { session with
          formula_store = Branch_solver.empty (* TODO: disallow some parents when they throw exceptions *)
        ; eval          = new_eval_session
        ; prev_sessions = session :: session.prev_sessions
        ; run_num       = session.run_num + 1 }
      in
      let rec next (session : t) : [ `Next of t | `Done of Branch.Status_store.t ] =
        match Branch.Status_store.get_unhit_branch session.branch_store with
        | None -> `Done session.branch_store
        | Some unhit -> begin
          match session.target_stack with
          | [] when session.run_num = 0 -> `Next (with_input_feeder session default_input_feeder)
          | [] -> `Done session.branch_store (* no targets left but some unhit branches, so [unhit] must be unreachable *)
          | target :: tl -> begin
            if
              Branch.Status_store.is_hit session.branch_store (Branch.Runtime.to_ast_branch target)
            then
              begin (* I'm surprised this is a syntax error without the begin/end *)
              Format.printf "Skipping already-hit target %s\n" (Branch.Runtime.to_string target);
              next { session with target_stack = tl }
              end
            else
              let fstore =
                Permanent_formulas.fold session.permanent_formulas ~init:session.formula_store ~f:(
                  fun acc expr ->
                    Branch_solver.add_formula [] Branch_solver.Parent.Global expr acc
                )
              in
              match Branch_solver.get_feeder target fstore with
              | Ok input_feeder -> `Next (with_input_feeder session input_feeder)
              | Error b ->
                (* mark as unsatisfiable and try the next target *)
                Format.printf "Unsatisfiable branch %s. Continuing to next target.\n" (Branch.Ast_branch.to_string b);
                let new_branch_store = 
                  target
                  |> Branch.Runtime.to_ast_branch
                  |> Branch.Status_store.set_branch_status ~new_status:Branch.Status.Unsatisfiable session.branch_store
                in
                next { session with target_stack = tl ; branch_store = new_branch_store }
            end
        end
      in
      next session

    let check_target_hit (session : t) (target : Branch.Runtime.t option) : bool =
      match target with
      | None -> true
      | Some target -> 
        target
        |> Branch.Runtime.to_ast_branch
        |> Branch.Status_store.is_hit session.branch_store

    let finish_and_print ({ branch_store ; _ } : t) : unit =
      branch_store
      |> Branch.Status_store.finish
      |> Branch.Status_store.print

    module Ref_cell =
      struct
        let hit_branch
          ?(new_status : Branch.Status.t = Branch.Status.Hit)
          (session : t ref)
          (branch : Branch.Runtime.t) (* Used to be ast branch, but can do more with runtime *)
          : unit
          =
          session := {
            !session with branch_store =
            Branch.Status_store.set_branch_status ~new_status (!session).branch_store
            @@ Branch.Runtime.to_ast_branch branch
          ; permanent_formulas =
              match new_status with
              | Found_abort ->
                branch
                |> Branch.Runtime.other_direction     
                |> Branch.Runtime.to_expr
                (* |> Branch_solver.gen_implied_formula [branch.condition_key] (!session).formula_store *)
                |> Permanent_formulas.add (!session).permanent_formulas
              | Branch.Status.Reached_max_step (* unimplemented *)
              | _ -> (!session).permanent_formulas
          }

        let add_key_eq_val
          (session : t ref)
          (parent : Branch_solver.Parent.t)
          (key : Lookup_key.t)
          (v : value)
          : unit
          =
          session := {
            !session with formula_store =
            (!session).formula_store
            |> Branch_solver.add_key_eq_val parent key v
          }

        let add_formula
          (session : t ref)
          (dependencies : Lookup_key.t list)
          (parent : Branch_solver.Parent.t)
          (formula : Z3.Expr.expr)
          : unit 
          =
          session := {
            !session with formula_store =
            (!session.formula_store)
            |> Branch_solver.add_formula dependencies parent formula
          }

        let add_siblings
          (session : t ref)
          (child_key : Lookup_key.t)
          (siblings : Lookup_key.t list)
          : unit 
          =
          session := {
            !session with formula_store =
            (!session.formula_store)
            |> Branch_solver.add_siblings child_key siblings
          }

        (* FIXME *)
        let update_target_branch
          (session : t ref)
          (hit_branch : Branch.Runtime.t)
          : unit
          =
          session := {
            !session with target_stack =
            let new_target = (* new target is the other side of the branch we just hit *)
              hit_branch
              |> Branch.Runtime.other_direction
            in
            match Branch.Status_store.get_status (!session).branch_store (Branch.Runtime.to_ast_branch new_target) with
            | Branch.Status.Unhit | Missed -> new_target :: (!session).target_stack (* FIXME: this can currently add duplicates *)
              (* CONSIDER: maybe have new status that is "enqueued" *)
              (* Maybe this solve will be different, so add missed targets again *)
            | Hit | Unreachable | Unsatisfiable | Found_abort | Reached_max_step ->
              (* Don't push new target if has already been considered *)
              (!session).target_stack
          }

        let exit_branch
          (session : t ref)
          (parent : Branch_solver.Parent.t)
          (exited_branch : Branch.Runtime.t)
          (ret_key : lookup_key.t)
          : unit
          =
          session := {
            !session with formula_store =
            (!session).formula_store
            |> branch_solver.exit_branch parent exited_branch ret_key
          }
      end

  end *)

module Concolic =
  struct
    module Outcome =
      struct
        type t =
          | Hit_target
          | Found_abort
          | Reach_max_step
      end

    (* TODO: set outcomes -- use outcomes *)
    type t =
      { branch_solver : Branch_solver.t
      ; cur_parent    : Branch_solver.Parent.t
      ; parent_stack  : Branch_solver.Parent.t list
      ; cur_target    : Branch.Runtime.t option
      ; new_targets   : Branch.Runtime.t list
      ; outcomes      : Outcome.t list
      ; hit_branches  : (Branch.Runtime.t * Branch.Status.t) list
      ; inputs        : (Ident.t * Dvalue.t) list }

    let create_default () : t =
      { branch_solver = Branch_solver.empty
      ; cur_parent    = Branch_solver.Parent.Global
      ; parent_stack  = []
      ; cur_target    = None
      ; new_targets   = []
      ; outcomes      = []
      ; hit_branches  = []
      ; inputs        = [] }

    let create ~(target : Branch.Runtime.t) ~(initial_formulas : Branch_solver.Formula_set.t) : t =
      let simple_add = Fn.flip (Branch_solver.add_formula [] Branch_solver.Parent.Global) in
      let branch_solver = Branch_solver.Formula_set.fold initial_formulas ~init:Branch_solver.empty ~f:simple_add in
      { (create_default ()) with branch_solver ; cur_target = Some target }

    let add_formula (session : t) (expr : Z3.Expr.expr) : t =
      { session with branch_solver = Branch_solver.add_formula [] session.cur_parent expr session.branch_solver }

    let add_key_eq_val (session : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      { session with branch_solver = Branch_solver.add_key_eq_val session.cur_parent key v session.branch_solver }

    let add_siblings (session : t) (key : Lookup_key.t) ~(siblings : Lookup_key.t list) : t =
      { session with branch_solver = Branch_solver.add_siblings key siblings session.branch_solver }

    let is_target (branch : Branch.Runtime.t) (target : Branch.Runtime.t option) : bool =
      Option.map target ~f:(fun x -> Branch.Runtime.compare branch x = 0)
      |> Option.value ~default:false

    let found_abort (session : t) : t =
      match session.cur_parent with
      | Branch_solver.Parent.Global -> session (* do nothing *)
      | Branch_solver.Parent.Local branch ->
        { session with hit_branches = (branch, Branch.Status.Found_abort) :: session.hit_branches }

    let enter_branch (session : t) (branch : Branch.Runtime.t) : t =
      { session with
        cur_parent   = Branch_solver.Parent.of_runtime_branch branch
      ; parent_stack = session.cur_parent :: session.parent_stack
      ; new_targets  = Branch.Runtime.other_direction branch :: session.new_targets
      ; hit_branches = (branch, Branch.Status.Hit) :: session.hit_branches
      ; outcomes     = (if is_target branch session.cur_target then [ Outcome.Hit_target ] else []) @ session.outcomes }

    let exit_branch (session : t) (ret_key : Lookup_key.t) : t =
      let new_parent, new_parent_stack =
        match session.parent_stack with
        | hd :: tl -> hd, tl
        | [] -> failwith "logically impossible to have no branch to exit"
      in
      let branch_solver =
        Branch_solver.exit_branch
          new_parent
          (Branch_solver.Parent.to_runtime_branch_exn session.cur_parent)
          ret_key
          session.branch_solver
      in
      { session with
        branch_solver
      ; parent_stack = new_parent_stack
      ; cur_parent = new_parent }

    let add_input (session : t) (x : Ident.t) (v : Dvalue.t) : t =
      { session with inputs = (x, v) :: session.inputs }

  end

module Target_stack =
  struct
    (* can see about saving less than the full session, but for now just save more than necessary *)
    type t = (Branch.Runtime.t * Concolic.t) list

    let empty = []
  end

type t = 
  { branch_store        : Branch.Status_store.t
  ; persistent_formulas : Branch_solver.Formula_set.t
  ; target_stack        : Target_stack.t
  ; global_max_step     : int
  ; run_num             : int }

let default_global_max_step = Int.(10 ** 3)

let create_default () : t =
  { branch_store        = Branch.Status_store.empty 
  ; persistent_formulas = Branch_solver.Formula_set.empty
  ; target_stack        = Target_stack.empty
  ; global_max_step     = default_global_max_step
  ; run_num             = 0 }

let load_branches (session : t) (expr : Jayil.Ast.expr) : t =
  { session with branch_store = Branch.Status_store.find_branches expr session.branch_store }

let inc_run_num (session : t) : t =
  { session with run_num = session.run_num + 1 }

let default_input_feeder : Input_feeder.t =
  fun _ -> Quickcheck.random_value ~seed:`Nondeterministic (Int.gen_incl (-10) 10)

let rec next (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] =
  let is_hit target =
    Branch.Status_store.is_hit session.branch_store
    @@ Branch.Runtime.to_ast_branch target
  in
  match Branch.Status_store.get_unhit_branch session.branch_store, session.target_stack with
  | None, _ -> (* all branches have been considered *)
    `Done session
  | _, [] when session.run_num > 0 -> (* no targets have been found that we can target next *)
    `Done session
  | _, [] -> (* no targets, but this is the first run, so use the default *)
    `Next ( inc_run_num session
          , Concolic.create_default ()
          , Eval.create default_input_feeder session.global_max_step )
  | _, (target, _) :: tl when is_hit target -> (* next target has already been hit *) (* TODO: consider matching on status *)
    Format.printf "Skipping already-hit target %s\n" (Branch.Runtime.to_string target);
    next { session with target_stack = tl }
  | _, (target, concolic_session) :: tl -> begin (* next target is unhit *)
    match Branch_solver.get_feeder target concolic_session.branch_solver with
    | Ok input_feeder ->
      `Next (inc_run_num session
            , Concolic.create ~target ~initial_formulas:session.persistent_formulas
            , Eval.create input_feeder session.global_max_step )
    | Error b ->
      Format.printf "Unsatisfiable branch %s. Continuing to next target.\n" (Branch.Ast_branch.to_string b);
      next { session with target_stack = tl }
    end

let finish (session : t) : t =
  { session with branch_store = Branch.Status_store.finish session.branch_store ; target_stack = Target_stack.empty }

let print ({ branch_store ; target_stack ; _ } : t) : unit =
  begin
  match target_stack with
  | (target, _) :: _ -> Branch.Runtime.print_target_option (Some target)
  | [] -> ()
  end;
  Branch.Status_store.print branch_store

(* TODO: check that target was hit by concolic *)
(* TODO: handle different types of hits from concolic *)
let accum_concolic (session : t) (concolic : Concolic.t) : t =
  let branch_store =
    concolic.hit_branches
    |> List.rev (* need to consider branches in the order they were hit *)
    |> List.fold ~init:session.branch_store ~f:(fun acc (branch, new_status) ->
      Branch.Status_store.set_branch_status ~new_status acc
      @@ Branch.Runtime.to_ast_branch branch
      )
  in
  let persistent_formulas =
    List.filter_map concolic.hit_branches ~f:(fun (b, s) ->
      match s with
      | Branch.Status.Found_abort ->
        Branch.Runtime.other_direction b
        |> Branch.Runtime.to_expr
        |> Option.return
      | _ -> None
    )
    |> Branch_solver.Formula_set.of_list
  in
  let new_targets = List.map concolic.new_targets ~f:(fun x -> (x, concolic)) in
  { session with
    branch_store 
  ; persistent_formulas = Branch_solver.Formula_set.join session.persistent_formulas persistent_formulas
  ; target_stack = new_targets @ session.target_stack }

