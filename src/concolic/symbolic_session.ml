
open Core
open Options.Fun.Infix

module Status =
  struct
    type t =
      | Found_abort of (Branch.t * Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jil_input.t list [@compare.ignore])
      | Finished_interpretation of { pruned : bool }
      [@@deriving compare, sexp]

    let prune (x : t) : t =
      match x with
      | Finished_interpretation _ -> Finished_interpretation { pruned = true }
      | _ -> x
  end

module Depth_tracker =
  struct
    type t =
      { cur_depth    : int (* branch depth *)
      ; max_depth    : int (* only for conditional branch depth *)
      ; is_max_step  : bool
      ; is_max_depth : bool } 
      (** [t] helps track if we've reached the max tree depth and thus should stop creating formulas *)

    let empty : t =
      { cur_depth    = 0
      ; max_depth    = Options.default.max_tree_depth
      ; is_max_step  = false
      ; is_max_depth = false }

    let with_options : (t, t) Options.Fun.a =
      Options.Fun.make
      @@ fun (r : Options.t) -> fun (x : t) -> { x with max_depth = r.max_tree_depth }

    let incr_branch (x : t) : t =
      { x with cur_depth = x.cur_depth + 1 ; is_max_depth = x.max_depth <= x.cur_depth }

    let hit_max_step (x : t) : t =
      { x with is_max_step = true }
  end

(* These don't change during the session, so keep them in one record to avoid so much copying *)
module Session_consts =
  struct
    type t =
      { target       : Target.t option
      ; input_feeder : Concolic_feeder.t
      ; max_step     : int } 

    let default : t =
      { target       = None
      ; input_feeder = Concolic_feeder.zero
      ; max_step     = Options.default.global_max_step }
  end

module T =
  struct
    type t =
      { stem           : Formulated_stem.t
      ; consts         : Session_consts.t
      ; status         : [ `In_progress | `Found_abort of Branch.t | `Type_mismatch | `Failed_assume ]
      ; rev_inputs     : Jil_input.t list
      ; depth_tracker  : Depth_tracker.t 
      ; latest_branch  : Branch.t option (* need to track the latest branch in order to report where an abort was found *)
      ; solvable_hit_branches : Branch.t list } (* this is different from latest branch because these are only non-const branches *)
  end

include T

let empty : t =
  { stem           = Formulated_stem.empty
  ; consts         = Session_consts.default
  ; status         = `In_progress
  ; rev_inputs     = []
  ; depth_tracker  = Depth_tracker.empty
  ; latest_branch  = None
  ; solvable_hit_branches = [] }

let with_options : (t, t) Options.Fun.a =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with depth_tracker = Options.Fun.appl Depth_tracker.with_options r x.depth_tracker
    ; consts = { x.consts with max_step = r.global_max_step } }

let get_max_step ({ consts = { max_step ; _ } ; _ } : t) : int =
  max_step

let get_feeder ({ consts = { input_feeder ; _ } ; _ } : t) : Concolic_feeder.t =
  input_feeder

let found_abort (s : t) : t =
  { s with status = `Found_abort (Option.value_exn s.latest_branch) } (* safe to get value b/c no aborts show up in global scope *)

let found_type_mismatch (s : t) : t =
  { s with status = `Type_mismatch }

let has_reached_target (x : t) : bool = 
  match x.consts.target with
  | Some target -> x.depth_tracker.cur_depth >= target.path_n
  | None -> true

let add_lazy_formula (x : t) (lazy_expr : unit -> Z3.Expr.expr) : t =
  if
    x.depth_tracker.is_max_depth
    || Fn.non has_reached_target x
    (* we don't add formula if we haven't yet reached the target because the formulas already exist in the path tree *)
  then x
  else { x with stem = Formulated_stem.push_formula x.stem @@ lazy_expr () }

(* require that cx is true by adding as formula *)
let found_assume (cx : Concolic_key.t) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.eqv cx (Jayil.Ast.Value_bool true)

let fail_assume (x : t) : t =
  if x.depth_tracker.is_max_depth
  then x
  else { x with status = `Failed_assume }

let hit_branch (branch : Branch.Runtime.t) (x : t) : t =
  let ast_branch = Branch.Runtime.to_ast_branch branch in
  if Concolic_key.is_const branch.condition_key
  then (* branch is constant and therefore isn't solvable. Just set as latest branch and push a formula for the branch *)
    add_lazy_formula { x with latest_branch = Some ast_branch }
    @@ fun () -> Branch.Runtime.to_expr branch
  else (* branch could be solved for, so add it as a branch to be later put in the tree, and say it was hit *)
    let after_incr = 
      { x with depth_tracker = Depth_tracker.incr_branch x.depth_tracker 
      ; latest_branch = Some ast_branch
      ; solvable_hit_branches = ast_branch :: x.solvable_hit_branches }
    in
    if after_incr.depth_tracker.is_max_depth || Fn.non has_reached_target x
    then after_incr
    else { after_incr with stem = Formulated_stem.push_branch after_incr.stem branch }

let reach_max_step (x : t) : t =
  { x with depth_tracker = Depth_tracker.hit_max_step x.depth_tracker }

(*
  ------------------------------
  FORMULAS FOR BASIC JIL CLAUSES
  ------------------------------
*)
let add_key_eq_val (key : Concolic_key.t) (v : Jayil.Ast.value) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.eqv key v

let add_alias (key1 : Concolic_key.t) (key2 : Concolic_key.t) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.eq key1 key2

let add_binop (key : Concolic_key.t) (op : Jayil.Ast.binary_operator) (left : Concolic_key.t) (right : Concolic_key.t) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.binop key op left right

let add_input (key : Concolic_key.t) (v : Dvalue.t) (x : t) : t =
  let n =
    match v with
    | Dvalue.Direct (Value_int n) -> n
    | _ -> failwith "non-int input" (* logically impossible *)
  in
  Dj_common.Log.Export.CLog.app (fun m -> m "Feed %d to %s \n" n (let Ident s = Concolic_key.id key in s));
  { x with rev_inputs = { clause_id = Concolic_key.id key ; input_value = n } :: x.rev_inputs }

let add_not (key1 : Concolic_key.t) (key2 : Concolic_key.t) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.not_ key1 key2

(*
  -----------------
  BETWEEN-RUN LOGIC   
  -----------------
*)

module Dead =
  struct
    type t =
      { tree    : Path_tree.t
      ; prev    : T.t }

    let of_sym_session : (T.t, Path_tree.t -> t) Options.Fun.a =
      Options.Fun.strong
        (fun (s : T.t) (f : bool -> Branch.t list -> Path_tree.t) (tree : Path_tree.t) -> 
          let failed_assume = match s.status with `Failed_assume -> true | _ -> false in
          let tree = 
            match s.consts.target with
            | None -> f failed_assume s.solvable_hit_branches (* use only solvable branches in path *)
            | Some target -> Path_tree.add_stem tree target s.stem failed_assume s.solvable_hit_branches
          in
          { tree ; prev = s })
        (Path_tree.of_stem <<^ (fun (s : T.t) -> s.stem))

    let root (x : t) : Path_tree.t =
      x.tree

    let get_status (x : t) : Status.t =
      match x.prev.status with
      | `In_progress | `Failed_assume ->
          let dt = x.prev.depth_tracker in
          Finished_interpretation { pruned = dt.is_max_depth || dt.is_max_step }
      | `Found_abort branch -> Found_abort (branch, List.rev x.prev.rev_inputs)
      | `Type_mismatch -> Type_mismatch (List.rev x.prev.rev_inputs)

    let is_reach_max_step (x : t) : bool =
      x.prev.depth_tracker.is_max_step
  end

(* Note that other side of all new targets are all the new hits *)
let[@landmarks] finish : (t, Path_tree.t -> Dead.t) Options.Fun.a =
  Dead.of_sym_session

let make (target : Target.t) (input_feeder : Concolic_feeder.t): t =
  { empty with consts = { empty.consts with target = Some target ; input_feeder }
  ; stem = Formulated_stem.of_target target }
