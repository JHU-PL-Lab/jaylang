open Core
open Dj_common
open Jayil.Ast
open Concolic_exceptions

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

    (* Say that x1 is an alias for x2. x1 is defined *after* x2 and points to x2. *)
    let add_alias (x1 : Id_with_stack.t) (x2 : Id_with_stack.t) ({ alias_graph; _ } : t) : unit =
      G.add_edge alias_graph x1 x2

    (* Say that x is the variable for the clause body that evaluates to dvalue *)
    let add_val_def_mapping (x : Id_with_stack.t) (vdef : (clause_body * Dvalue.t)) ({ val_def_map; _ } : t) : unit =
      Hashtbl.add_exn ~key:x ~data:vdef val_def_map

  end

(*
    If a record has a field that itself has mutable fields, those inner fields can change
    and the outer record does not have to have mutable fields.
    e.g. type m = { x : int ref }
    type r = { y : m }
    let z = { y = { x = ref 0 }}
    let zy = z.y
    zy.x := 1
    Now z is { y = { x = ref 1 }}, even though z has no ref cells 

  TODO: maybe I do want to make the session with mutable fields so that it doesn't have to pass
  it along in eval, since already we don't pass along the eval session. It feels weird to pass one
  but not the other.
    So basically concolic is just a wrapper for the variables. At that point, why even have a record
    instead of just mutable members of the module? Because that's ugly and the module is trying to be
    a class at that point.
  OR I have a reference to a concolic session, and the interp updates the session.
    I can then let Session.t = Concolic.t ref, and it has the necessary wrappers.

  TODO: because I end up splitting them up anyways, maybe I want to not nest them
    and instead I pass them around separately. I just need to think about the `next` implications.
    They are currently coupled because of how the concolic session affects the eval session's feeder.
*)
module Concolic =
  struct
    (*
      The concolic session remembers the branches and formulas. These are additional
      fields that are not needed during the regular evaluation.

      The concolic session actually wraps the eval session, which is mutable
    *)
    type t =
      { branch_store    : Ast_branch.Status_store.t
      ; formula_store   : Branch_solver.t
      ; target_stack    : Branch_solver.Target.t list
      ; prev_sessions   : t list
      ; global_max_step : int
      ; run_num         : int 
      ; eval            : Eval.t } 

    let load_branches (session : t) (e : expr) : t =
      { session with branch_store = Ast_branch.Status_store.find_branches e session.branch_store }

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
      { branch_store    = Ast_branch.Status_store.empty
      ; formula_store   = Branch_solver.empty 
      ; target_stack    = []
      ; prev_sessions   = []
      ; global_max_step = default_global_max_step
      ; run_num         = 0 
      ; eval            = create_eval default_input_feeder default_global_max_step }

    let reset_formulas (session : t) : t =
      { session with formula_store = Branch_solver.empty }

    (*
      The next session...
      * keeps the branch store because we want to remember which have been hit
      * keeps the target that was found in the previous run
      * increments the run number
      * resets the formula store because formulas are dependent on inputs, which will be different in the next run.
        * TODO: which formulas can we keep? We might want to carry over *some* information
      * creates a brand new eval session
      * uses the target stack to determine the next input feeder

      A few things can happen
      * We could have hit the max step, which we can check by comparing step with max step
        (might prefer to have a flag for this)
        * In which case we need to back up to the previous session because this one was effectively
          "unsatisfiable" and cannot provide any new information
      * We could have properly hit the previous target branch
      * We can try to find an input feeder for the next target and succeed
      * We can try to find an input feeder for the next target and fail
        * in which case we need to record that and move onto the next target

      TODO: use variants instead of exceptions?
    *)
    let next (session : t) : [ `Next of t | `Done of Ast_branch.Status_store.t ] =
      let with_input_feeder (session : t) (input_feeder : Input_feeder.t) : t =
        let new_eval_session = create_eval input_feeder session.global_max_step in
        { session with
          formula_store = Branch_solver.empty
        ; eval          = new_eval_session
        ; prev_sessions = session :: session.prev_sessions
        ; run_num       = session.run_num + 1 }
      in
      let rec next (session : t) : [ `Next of t | `Done of Ast_branch.Status_store.t ] =
        match Ast_branch.Status_store.get_unhit_branch session.branch_store with
        | None -> `Done session.branch_store
        | Some unhit -> begin
          match session.target_stack with
          | [] when session.run_num = 0 -> `Next (with_input_feeder session default_input_feeder)
          | [] -> `Done session.branch_store (* no targets left but some unhit branches, so [unhit] must be unreachable *)
          | target :: tl -> begin
            if
              Ast_branch.Status_store.is_hit session.branch_store (Branch_solver.Target.to_branch target)
            then
              begin (* I'm surprised this is a syntax error without the begin/end *)
              Format.printf "Skipping already-hit target %s\n" (Branch_solver.Target.to_string target);
              next { session with target_stack = tl }
              end
            else
              match Branch_solver.get_feeder target session.formula_store with
              | `Ok input_feeder -> `Next (with_input_feeder session input_feeder)
              | `Unsatisfiable_branch b ->
                (* mark as unsatisfiable and try the next target *)
                Format.printf "Unsatisfiable branch %s. Continuing to next target.\n" (Ast_branch.to_string b);
                let new_branch_store = 
                  target
                  |> Branch_solver.Target.to_branch
                  |> Ast_branch.Status_store.set_unsatisfiable session.branch_store
                in
                (* FIXME: when last branch is unsatisfiable, the setting doesn't propogate and it stays unhit *)
                next { session with target_stack = tl ; branch_store = new_branch_store }
            end
        end
      in
      next session

    let assert_target_hit (session : t) (target : Branch_solver.Target.t option) : unit =
      match target with
      | None -> ()
      | Some target -> 
        target
        |> Branch_solver.Target.to_branch
        |> Ast_branch.Status_store.is_hit session.branch_store
        |> function
          | true -> ()
          | false -> raise Missed_Target_Branch

    let finish_and_print ({ branch_store ; _ } : t) : unit =
      branch_store
      |> Ast_branch.Status_store.finish
      |> Ast_branch.Status_store.print

    module Ref_cell =
      struct
        let add_key_eq_value_opt
          (session : t ref)
          (parent : Branch_solver.Parent.t)
          (key : Lookup_key.t)
          (v_opt : value option)
          : unit
          =
          session := {
            !session with formula_store =
            (!session).formula_store
            |> Branch_solver.add_key_eq_value_opt parent key v_opt
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

        let update_target_branch
          (session : t ref)
          (branch_key : Lookup_key.t)
          (hit_branch : Branch_solver.Runtime_branch.t)
          : unit
          =
          session := {
            !session with target_stack =
            let new_target = (* new target is the other side of the branch we just hit *)
              hit_branch
              |> Branch_solver.Runtime_branch.other_direction
              |> fun branch -> Branch_solver.Target.{ branch_key ; branch }
            in
            match Ast_branch.Status_store.get_status (!session).branch_store (Branch_solver.Target.to_branch new_target) with
            | Ast_branch.Status.Unhit -> new_target :: (!session).target_stack (* FIXME: this can currently add duplicates *)
              (* CONSIDER: maybe have new status that is "enqueued" *)
            | Hit | Unreachable | Unsatisfiable | Reached_max_step ->
              (* Don't push new target if has already been considered *)
              (!session).target_stack
(* 
            if not @@ Ast_branch.Status_store.is_hit (!session).branch_store (Branch_solver.Target.to_branch new_target)
             (!session).target_stack (* new target is already hit, so don't push new target *)
            else new_target :: (!session).target_stack *)
          }

        let exit_branch
          (session : t ref)
          (branch_key : Lookup_key.t)
          (parent : Branch_solver.Parent.t)
          (exited_branch : Branch_solver.Runtime_branch.t)
          (ret_key : Lookup_key.t)
          : unit
          =
          session := {
            !session with formula_store =
            (!session).formula_store
            |> Branch_solver.exit_branch branch_key parent exited_branch ret_key
          }
      end

  end