open Core
open Path_tree
open Dj_common
open Jayil.Ast

module Deprecated =
  struct
    (*
      Mutable record that tracks a run through the evaluation. aka "interpreter session"
    *)
    module Eval =
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

    module Concolic =
      struct

        type t =
          { formula_tracker : Formula_tracker.t
          ; branch_tracker  : Branch_tracker.Runtime.t
          ; input           : Branch_tracker.Input.t
          ; has_hit_exit    : bool
          ; has_hit_abort   : bool } (* quick patch for option to quit on first abort *)

        let default : t =
          { formula_tracker = Formula_tracker.empty
          ; branch_tracker  = Branch_tracker.Runtime.empty
          ; input           = []
          ; has_hit_exit    = false
          ; has_hit_abort   = false }

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
          ; branch_tracker = Branch_tracker.Runtime.found_abort session.branch_tracker
          ; has_hit_exit = true
          ; has_hit_abort = true }

        let reach_max_step (session : t) : t =
          { session with
            formula_tracker = Formula_tracker.exit_until_global session.formula_tracker
          ; branch_tracker = Branch_tracker.Runtime.reach_max_step session.branch_tracker
          ; has_hit_exit = true }

        let fail_assume (session : t) (cx : Lookup_key.t) : t =
          { session with
            formula_tracker =  (* add a found_assume into formula tracker *)
              Formula_tracker.add_formula session.formula_tracker (Riddler.eqv cx (Jayil.Ast.Value_bool true))
              |> Formula_tracker.exit_until_global
          ; has_hit_exit = true }

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
      ; run_num          : int
      ; has_hit_exit     : bool (* true iff some concolic run has hit exiting control flow *)
      ; is_done          : bool (* temporary patch to hard set a session to done if the concolic session finds no inputs, or if hits abort and should quit *)
      ; quit_on_first_abort : bool } (* quick patch. See concolic session above *)

    let default_global_max_step = Int.(2 * 10 ** 3)

    let default_solver_timeout_s = 1.0

    let default : t =
      { branch_tracker   = Branch_tracker.empty
      ; formula_tracker  = Formula_tracker.empty
      ; solver_timeout_s = default_solver_timeout_s
      ; global_max_step  = default_global_max_step
      ; run_num          = 0
      ; has_hit_exit     = false
      ; is_done          = false
      ; quit_on_first_abort = true }

    let with_options
      ?(solver_timeout_sec : float = default_solver_timeout_s)
      ?(global_max_step : [ `Const of int | `Scale of float ] = `Const default_global_max_step)
      ?(quit_on_first_abort : bool = true)
      (session : t)
      : t
      =
      { session with
        quit_on_first_abort
      ; solver_timeout_s = solver_timeout_sec
      ; global_max_step = match global_max_step with `Const x -> x | _ -> failwith "unsupported scaling max step" }

    let set_quit_on_first_abort (session : t) (b : bool) : t =
      { session with quit_on_first_abort = b }

    let of_expr (expr : Jayil.Ast.expr) : t =
      { default with branch_tracker = Branch_tracker.of_expr expr }

    let rec next (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] Lwt.t =
      let%lwt () = Lwt.pause () in (* allows for lwt timeouts at this point *)
      if session.is_done then Lwt.return (`Done session) else
      match Branch_tracker.next_target session.branch_tracker with
      | None, branch_tracker when session.run_num > 0 -> Lwt.return (`Done { session with branch_tracker })
      | None, branch_tracker -> (* no targets, but this is the first run, so use the default *)
        `Next ({ session with run_num = session.run_num + 1 ; branch_tracker }
              , Concolic.default
              , Eval.create Concolic_feeder.default session.global_max_step )
        |> Lwt.return
      | Some target, branch_tracker ->
        solve_for_target target { session with branch_tracker }

    and solve_for_target (target : Branch.t) (session : t) : [ `Done of t | `Next of t * Concolic.t * Eval.t ] Lwt.t =
      let%lwt () = Lwt.pause () in
      Solver.set_timeout_sec Solver.SuduZ3.ctx (Some (Core.Time_float.Span.of_sec session.solver_timeout_s));
      Format.printf "Solving for %s\n" (Branch.to_string target);
      match Branch_solver.check_solver target session.formula_tracker session.branch_tracker with
      | `Unsolvable when session.has_hit_exit ->
        next { session with branch_tracker = Branch_tracker.set_unknown session.branch_tracker target }
      | `Timeout ->
        next { session with branch_tracker = Branch_tracker.set_unknown session.branch_tracker target }
      | `Unsolvable ->
        next { session with branch_tracker = Branch_tracker.set_status session.branch_tracker target Branch_tracker.Status.Unsatisfiable }
      | `Solved model ->
        `Next ({ session with run_num = session.run_num + 1 }
              , Concolic.create ~target ~formula_tracker:Formula_tracker.empty
              , Eval.create (Concolic_feeder.from_model model) session.global_max_step )
        |> Lwt.return

    let finish (session : t) : t =
      { session with branch_tracker = Branch_tracker.finish session.branch_tracker session.has_hit_exit }

    let print ({ branch_tracker ; _ } : t) : unit =
      Branch_tracker.print branch_tracker

    let accum_concolic (session : t) (concolic : Concolic.t) : t =
      { session with
        formula_tracker = Formula_tracker.merge session.formula_tracker concolic.formula_tracker
      ; branch_tracker = Branch_tracker.collect_runtime session.branch_tracker concolic.branch_tracker concolic.input
      ; has_hit_exit = session.has_hit_exit || concolic.has_hit_exit
      ; is_done = List.is_empty concolic.input || (session.quit_on_first_abort && concolic.has_hit_abort) }

    let branch_tracker ({ branch_tracker ; _ } : t) : Branch_tracker.t =
      branch_tracker
    
  end (* Deprecated *)


(*
  --------------
  END DEPRECATED   
  --------------
*)

module Concrete = Deprecated.Eval (* not actually deprecated *)

module Symbolic =
  struct

    (* Node_stack is a stack of nodes that describes a path through the tree. This will be used by the symbolic session. *)
    module Node_stack =
      struct
        type t =
          | Last of Root.t
          | Cons of Child.t * t 

        let empty : t = Last Root.empty

        (* To avoid extra children and to keep the stack a single path, begin with only the formulas (and discard the children) from root. *)
        let of_root (root : Root.t) : t =
          Last (Root.with_formulas Root.empty root.formulas)

        let hd_node : t -> Root.t = function
          | Last node -> node
          | Cons (child, _) -> Child.to_node_exn child

        let hd_branch_exn : t -> Branch.t = function
          | Last _ -> failwith "no hd branch"
          | Cons (child, _) -> Branch.Runtime.to_ast_branch child.branch

        let map_hd (stack : t) ~(f : Node.t -> Node.t) : t =
          match stack with
          | Last base -> Last (f base)
          | Cons (hd, tl) -> Cons (Child.map_node hd ~f, tl) (* might be a good choice to throw exception if node doesn't exist *)

        (* Creates a tree that is effectively the path down the node stack *)
        let to_tree (stack : t) : Root.t =
          let rec to_tree acc = function
            | Last root -> { root with children = acc }
            | Cons ({ status = Hit node ; _} as child, tl) -> (* branch was hit, and no failed assert or assume *)
              let acc = Children.set_node Children.empty child.branch { node with children = acc } in
              to_tree acc tl
            | Cons ({ status = Unsolved ; _} as child, tl) -> (* branch was hit, and failed assert of assume *)
              assert (Children.is_empty acc);
              let acc = Children.set_child Children.empty child in (* has no node in which to set children *)
              to_tree acc tl
            | _ -> failwith "logically impossible" (* impossible if the code does as I expect it *) 
          in
          to_tree Children.empty stack

        (* Gives a root-to-leaf path by reversing the stack *)
        let to_path (x : t) : Path.t =
          let rec loop acc = function
            | Last _ -> acc
            | Cons (child, tl) -> loop (child.branch :: acc) tl
          in
          loop [] x

        let get_targets (stack : t) (tree : Root.t) : Target.t list =
          let total_path = to_path stack in (* this path leads to all possible targets *)
          let rec step
            (cur_targets : Target.t list)
            (cur_node : Node.t)
            (remaining_path : Path.t)
            (depth : int)
            : Target.t list
            =
            match remaining_path with
            | last_branch :: [] -> (* maybe last node hit should be target again in case it failed assume or assert *)
              push_target
                (push_target cur_targets cur_node last_branch depth) (* push this direction *)
                cur_node
                (Branch.Runtime.other_direction last_branch) (* push other direction *)
                depth
            | branch :: tl ->
              step
                (push_target cur_targets cur_node (Branch.Runtime.other_direction branch) depth) (* push other direction as possible target *)
                (Child.to_node_exn @@ Node.get_child_exn cur_node branch) (* step down path *)
                tl (* continue down remainder of path *)
                (depth + 1)
            | [] -> cur_targets
          and push_target
            (cur_targets : Target.t list)
            (cur_node : Node.t)
            (branch : Branch.Runtime.t)
            (depth : int)
            : Target.t list
            =
            if Node.is_valid_target_child cur_node branch
            then Target.create (Node.get_child_exn cur_node branch) total_path depth :: cur_targets
            else cur_targets
          in
          step [] tree total_path 0

        (* Note that merging two trees would have to visit every node in both of them in the worst case,
          but we know that the tree made from the stack is a single path, so it only has to merge down
          that single path because only one child is hit and needs to be merged (see Path_tree.Status.merge). *)
        let merge_with_tree (allowed_tree_depth : int) (stack : t) (tree : Root.t) : Root.t * Target.t list =
          let merged =
            Root.merge tree
            @@ to_tree stack
          in
          merged, get_targets stack merged

        (* pushes node reached by [branch] onto the stack *)
        let push (stack : t) (branch : Branch.Runtime.t) : t =
          Cons (Child.create Node.empty branch, stack)

        (* adds formula to top node of the stack *)
        let add_formula (stack : t) (expr : Z3.Expr.expr) : t =
          map_hd stack ~f:(fun node -> Node.add_formula node expr)
      end (* Node_stack *)

    module Depth_logic =
      struct
        type t =
          { cur_depth    : int
          ; max_depth    : int
          ; is_below_max : bool } 
          (** [t] helps track if we've reached the max tree depth and thus should stop creating formulas *)

        let empty (max_depth : int) : t =
          { cur_depth = 0; max_depth ; is_below_max = true }

        let incr (x : t) : t =
          { x with cur_depth = x.cur_depth + 1 ; is_below_max = x.cur_depth < x.max_depth }
      end

    module Branch_set = Set.Make (Branch)

    type t =
      { stack          : Node_stack.t
      ; target         : Target.t option
      ; branch_info    : Branch_info.t
      ; depth          : Depth_logic.t }
      (* TODO: track a path in a tree and only add formulas if at a new node. TODO: add a "pruned" variant to path tree status *)
      (* FIXME! The wrong branch is aborted when we have hit max depth *)
      (* Need to have two variants of this: one is below max depth, the other is not. The one above max depth just has a branch stack *)

    let empty : t =
      { stack          = Node_stack.empty
      ; target         = None
      ; branch_info    = Branch_info.empty
      ; depth          = Depth_logic.empty Concolic_options.default.max_tree_depth }

    let with_options : (t -> t) Concolic_options.Fun.t =
      Concolic_options.Fun.make
      @@ fun (r : Concolic_options.t) -> fun (x : t) -> { x with depth = { x.depth with max_depth = r.max_tree_depth } }


    (* [lazy_expr] does not get evaluated unless [x] is below max depth *)
    let add_lazy_formula (x : t) (lazy_expr : unit -> Z3.Expr.expr) : t =
      if x.depth.is_below_max
      then { x with stack = Node_stack.add_formula x.stack @@ lazy_expr () }
      else x

    let hit_branch (x : t) (branch : Branch.Runtime.t) : t =
      let without_formulas =
        { x with branch_info = Branch_info.set_branch_status ~new_status:Hit x.branch_info @@ Branch.Runtime.to_ast_branch branch }
      in
      if x.depth.is_below_max
      then
        { without_formulas with
          stack = Node_stack.add_formula (Node_stack.push without_formulas.stack branch) @@ Branch.Runtime.to_expr branch
        ; depth = Depth_logic.incr without_formulas.depth }
      else
        without_formulas

    let fail_assume (x : t) (cx : Lookup_key.t) : t =
      match x.stack with
      | Last _ -> x (* `assume` found in global scope. We assume this is a test case that can't happen in real world translations to JIL *)
      | _ when not x.depth.is_below_max -> x
      | Cons (hd, tl) ->
        let hd = Child.map_node hd ~f:(fun node -> Node.add_formula node @@ Riddler.eqv cx (Jayil.Ast.Value_bool true)) in
        let new_hd =
          Child.{ status = Status.Unsolved (* forget all formulas so that it is a possible target in future runs *)
                ; constraints = Formula_set.add_multi hd.constraints @@ Child.to_formulas hd (* constrain to passing assume/assert *)
                ; branch = hd.branch }
        in
        { x with stack = Cons (new_hd, tl) }

    let found_abort (x : t) : t =
      { x with branch_info = Branch_info.set_branch_status ~new_status:Found_abort x.branch_info @@ Node_stack.hd_branch_exn x.stack }

    let reach_max_step (x : t) : t =
      x

    (*
      ------------------------------
      FORMULAS FOR BASIC JIL CLAUSES
      ------------------------------
    *)
    let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      add_lazy_formula x @@ fun () -> Riddler.eq_term_v key (Some v)

    let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      add_lazy_formula x @@ fun () -> Riddler.eq key1 key2

    let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
      add_lazy_formula x @@ fun () -> Riddler.binop_without_picked key op left right

    let add_input (x : t) (key : Lookup_key.t) (v : Dvalue.t) : t =
      let Ident s = key.x in
      let n =
        match v with
        | Dvalue.Direct (Value_int n) -> n
        | _ -> failwith "non-int input" (* logically impossible *)
      in
      if Printer.print then Format.printf "Feed %d to %s \n" n s;
      add_lazy_formula x @@ fun () -> Riddler.if_pattern key Jayil.Ast.Int_pattern

    let add_not (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      add_lazy_formula x @@ fun () -> Riddler.not_ key1 key2

    let add_match (x : t) (k : Lookup_key.t) (m : Lookup_key.t) (pat : Jayil.Ast.pattern) : t =
      add_lazy_formula x
      @@ fun () ->
        let k_expr = Riddler.key_to_var k in
        Solver.SuduZ3.eq (Solver.SuduZ3.project_bool k_expr) (Riddler.if_pattern m pat)

    (*
      -----------------
      BETWEEN-RUN LOGIC   
      -----------------
    *)

    (* Note that other side of all new targets are all the new hits *)
    let[@landmarks] finish (x : t) (tree : Root.t) : Root.t * Target.t list =
      (* logically sound to have hit target if formulas are consistent with JIL program *)
      assert (Option.is_none x.target || Target.is_hit (Option.value_exn x.target) x.branch_info);
      let root, targets = Node_stack.merge_with_tree x.depth.max_depth x.stack tree in
      root, targets

    let next (max_depth : int) (root : Root.t) (target : Target.t) : t =
      { empty with
        stack = Node_stack.of_root root
      ; target = Some target
      ; depth = Depth_logic.empty max_depth }
    
  end

type t =
  { tree         : Root.t (* pointer to the root of the entire tree of paths *)
  ; target_queue : Target_queue.t
  ; symbolic     : Symbolic.t
  ; branch_info  : Branch_info.t
  ; run_num      : int
  ; options      : Concolic_options.t
  ; quit         : bool
  ; has_pruned   : bool } (* true iff some evaluation hit more nodes than are allowed to be kept *)

let empty : t =
  { tree         = Root.empty
  ; target_queue = Target_queue.empty
  ; symbolic     = Symbolic.empty
  ; branch_info  = Branch_info.empty
  ; run_num      = 0
  ; options      = Concolic_options.default
  ; quit         = false
  ; has_pruned   = false }

let with_options : (t -> t) Concolic_options.Fun.t =
  Concolic_options.Fun.make
  @@ fun (r : Concolic_options.t) -> fun (x : t) ->
    { x with options = r ; symbolic = Concolic_options.Fun.appl Symbolic.with_options r x.symbolic }

let of_expr (expr : Jayil.Ast.expr) : t =
  { empty with branch_info = Branch_info.of_expr expr }

let accum_symbolic (x : t) (sym : Symbolic.t) : t =
  let tree, new_targets = Symbolic.finish sym x.tree in
  { x with tree
  ; has_pruned   = x.has_pruned || not sym.depth.is_below_max
  ; branch_info  = Branch_info.merge x.branch_info sym.branch_info
  ; target_queue = Target_queue.push_list x.target_queue new_targets
  ; quit         = x.quit || x.options.quit_on_abort && Branch_info.contains sym.branch_info Found_abort }

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

(* $ OCAML_LANDMARKS=on ./_build/... *)
(* TODO: print or return that tree was pruned *)
let[@landmarks] rec next (x : t) : [ `Done of Branch_info.t | `Next of (t * Symbolic.t * Concrete.t) ] =
  if x.quit then `Done x.branch_info else
  (* It's never realistically relevant to quit when all branches are hit because at least one will have an abort *)
  (* if Branch_tracker.Status_store.Without_payload.all_hit x.branches then `Done x.branches else *)
  match Target_queue.pop x.target_queue with
  | Some (target, target_queue) -> 
    solve_for_target { x with target_queue } target
  | None when x.run_num = 0 ->
    `Next ({ x with run_num = 1 }, Symbolic.empty, Concrete.create Concolic_feeder.default x.options.global_max_step)
  | None -> (* no targets left, so done *)
    `Done x.branch_info

and solve_for_target (x : t) (target : Target.t) =
  let t0 = Caml_unix.gettimeofday () in
  let new_solver = load_solver (make_solver ()) (Target.to_formulas target x.tree) in
  Solver.set_timeout_sec Solver.SuduZ3.ctx (Some (Core.Time_float.Span.of_sec x.options.solver_timeout_sec));
  if x.options.print_solver then
    begin
    Format.printf "Solving for target %s\n" (Branch.Runtime.to_string target.child.branch);
    (* Format.printf "Path is%s\n" (List.to_string target.path ~f:(Branch.Runtime.to_string_short)) *)
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
      , Symbolic.next x.options.max_tree_depth x.tree target
      , Z3.Solver.get_model new_solver
        |> Core.Option.value_exn
        |> Concolic_feeder.from_model
        |> fun feeder -> Concrete.create feeder x.options.global_max_step
    )

let branch_info ({ branch_info ; _ } : t) : Branch_info.t =
  branch_info

let run_num ({ run_num ; _ } : t) : int =
  run_num