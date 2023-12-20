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

module Concolic =
  struct
    module Outcome =
      struct
        type t =
          | Hit_target
          | Found_abort
          | Reach_max_step
          [@@deriving compare, sexp]
      end

    module Outcome_set = Set.Make (Outcome)

    (* TODO: set outcomes -- use outcomes *)
    type t =
      { branch_solver : Branch_solver.t
      ; cur_parent    : Branch_solver.Parent.t
      ; parent_stack  : Branch_solver.Parent.t list
      ; cur_target    : Branch.Runtime.t option
      ; new_targets   : Branch.Runtime.t list
      ; outcomes      : Outcome_set.t
      ; hit_branches  : (Branch.Runtime.t * Branch.Status.t) list
      ; inputs        : (Ident.t * Dvalue.t) list }

    let create_default () : t =
      { branch_solver = Branch_solver.empty
      ; cur_parent    = Branch_solver.Parent.Global
      ; parent_stack  = []
      ; cur_target    = None
      ; new_targets   = []
      ; outcomes      = Outcome_set.singleton Outcome.Hit_target (* hacky: say no target means target is always hit *)
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
      | Branch_solver.Parent.Global -> session (* do nothing -- should be logically impossible *)
      | Branch_solver.Parent.Local branch ->
        { session with
          hit_branches = (branch, Branch.Status.Found_abort) :: session.hit_branches
        ; outcomes = Set.add session.outcomes Outcome.Found_abort }

    let enter_branch (session : t) (branch : Branch.Runtime.t) : t =
      Printf.printf "Hitting: %s: %s\n"
        (let (Jayil.Ast.Ident x) = branch.branch_key.x in x)
        (Branch.Direction.to_string branch.direction);
      { session with
        cur_parent   = Branch_solver.Parent.of_runtime_branch branch
      ; parent_stack = session.cur_parent :: session.parent_stack
      ; new_targets  = Branch.Runtime.other_direction branch :: session.new_targets
      ; hit_branches = (branch, Branch.Status.Hit) :: session.hit_branches
      ; outcomes     = if is_target branch session.cur_target then Set.add session.outcomes Outcome.Hit_target else session.outcomes }

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
      let Ident s = x in
      let n =
        match v with
        | Dvalue.Direct (Value_int n) -> n
        | _ -> failwith "non-int input" (* logically impossible *)
      in
      Format.printf "Feed %d to %s \n" n s;
      { session with inputs = (x, v) :: session.inputs }

    let has_abort (session : t) : bool =
      Set.mem session.outcomes Outcome.Found_abort

    let has_hit_target (session : t) : bool =
      Set.mem session.outcomes Outcome.Hit_target

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
      let branch_store = 
        Branch.Status_store.set_branch_status
          ~new_status:Branch.Status.Unsatisfiable
          session.branch_store
          b
      in
      next { session with target_stack = tl ; branch_store }
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
        Format.printf "Creating persistent formula for branch %s\n" (Branch.Runtime.to_ast_branch b |> Branch.Ast_branch.to_string);
        Branch.Runtime.other_direction b
        |> Branch.Runtime.to_expr
        |> Option.return
      | _ -> None
    )
    |> Branch_solver.Formula_set.of_list
  in
  let target_stack = 
    let map_targets = List.map ~f:(fun target -> (target, concolic)) in
    match Concolic.has_hit_target concolic, Concolic.has_abort concolic with
    | false, true -> session.target_stack (* didn't make enough progress to add meaningful targets *)
    | true, _ -> map_targets concolic.new_targets @ session.target_stack (* has meaningful targets, so prioritize them *)
    | false, _ -> failwith "unimplemented missed target" (* TODO: set status and continue reasonably *)
  in
  { session with
    branch_store 
  ; target_stack
  ; persistent_formulas = Branch_solver.Formula_set.join session.persistent_formulas persistent_formulas }

