
open Core
open Path_tree

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

    let hd_branch : t -> Branch.Or_global.t = function
      | Last _ -> Branch.Or_global.Global
      | Cons (child, _) -> Branch (Branch.Runtime.to_ast_branch child.branch)

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
        | Cons ({ status = Failed_assume ; _} as child, tl) -> (* branch was hit, and failed assert or assume *)
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
      Path.return
      @@ loop [] x

    (* Returns list of targets where deeper branches are at the front of the list *)
    let get_targets (stack : t) (tree : Root.t) : Target.t list =
      let total_path = to_path stack in (* this path leads to all possible targets *)
      let rec step
        (cur_targets : Target.t list)
        (cur_node : Node.t)
        (remaining_path : Path.t)
        (path : Path.t)
        : Target.t list
        =
        match remaining_path.forward_path with
        | last_branch :: [] -> (* maybe last node hit should be target again in case it failed assume or assert *)
          push_target
            (push_target cur_targets cur_node last_branch path) (* push this direction *)
            cur_node
            (Branch.Runtime.other_direction last_branch) (* push other direction *)
            path
        | branch :: tl ->
          step
            (push_target cur_targets cur_node (Branch.Runtime.other_direction branch) path) (* push other direction as possible target *)
            (Child.to_node_exn @@ Node.get_child_exn cur_node branch) (* step down path *)
            (Path.return tl) (* continue down remainder of path *)
            (Path.extend path [ branch ]) (* complexity of this is bad, but depth is so shallow that is not slow in practice *)
        | [] -> cur_targets
      and push_target
        (cur_targets : Target.t list)
        (cur_node : Node.t)
        (branch : Branch.Runtime.t)
        (path : Path.t)
        : Target.t list
        =
        if Node.is_valid_target_child cur_node branch
        then Target.create branch path :: cur_targets
        else cur_targets
      in
      step [] tree total_path Path.empty

    (* Note that merging two trees would have to visit every node in both of them in the worst case,
      but we know that the tree made from the stack is a single path, so it only has to merge down
      that single path because only one child is hit and needs to be merged (see Path_tree.Status.merge). *)
    let merge_with_tree (stack : t) (tree : Root.t) : Root.t * Target.t list =
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

    let with_options : (t, t) Options.Fun.t =
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
      ; input_feeder = Concolic_feeder.default
      ; max_step     = Options.default.global_max_step }
  end


module T =
  struct
    type t =
      { stack          : Node_stack.t
      ; consts         : Session_consts.t
      ; status         : [ `In_progress | `Found_abort of Branch.t | `Type_mismatch ]
      ; rev_inputs     : Jil_input.t list
      ; depth_tracker  : Depth_tracker.t 
      ; latest_branch  : Branch.t option }
  end

include T

let empty : t =
  { stack          = Node_stack.empty
  ; consts         = Session_consts.default
  ; status         = `In_progress
  ; rev_inputs     = []
  ; depth_tracker  = Depth_tracker.empty
  ; latest_branch  = None }

let with_options : (t, t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (x : t) ->
    { x with depth_tracker = Options.Fun.run Depth_tracker.with_options r x.depth_tracker
    ; consts = { x.consts with max_step = r.global_max_step } }

let get_max_step ({ consts = { max_step ; _ } ; _ } : t) : int =
  max_step

let get_feeder ({ consts = { input_feeder ; _ } ; _ } : t) : Concolic_feeder.t =
  input_feeder

let found_abort (s : t) : t =
  { s with status = `Found_abort (Option.value_exn s.latest_branch) } (* safe to get value b/c no aborts show up in global scope *)

let found_type_mismatch (s : t) : t =
  { s with status = `Type_mismatch }

(* require that cx is true by adding as formula *)
let found_assume (cx : Concolic_key.t) (x : t) : t =
  if x.depth_tracker.is_max_depth
  then x
  else
    match x.stack with
    | Last _ -> x (* `assume` found in global scope. We assume this is a test case that can't happen in real world translations to JIL *)
    | Cons (hd, tl) ->
      let new_hd = Child.map_node hd ~f:(fun node -> Node.add_formula node @@ Concolic_riddler.eqv cx (Jayil.Ast.Value_bool true)) in
      { x with stack = Cons (new_hd, tl) }

let fail_assume (x : t) : t =
  if x.depth_tracker.is_max_depth
  then x
  else
    match x.stack with
    | Last _ -> x (* `assume` found in global scope. We assume this is a test case that can't happen in real world translations to JIL *)
    | Cons (hd, tl) ->
      let new_hd =
        Child.{ status = Path_tree.Status.Failed_assume (* forget all formulas so that it is a possible target in future runs *)
              ; constraints = Formula_set.add_multi hd.constraints @@ Child.to_formulas hd (* constrain to passing assume/assert *)
              ; branch = hd.branch }
      in
      { x with stack = Cons (new_hd, tl) }

let add_lazy_formula (x : t) (lazy_expr : unit -> Z3.Expr.expr) : t =
  if
    x.depth_tracker.is_max_depth
    || begin
      match x.consts.target with
      | Some target -> x.depth_tracker.cur_depth < target.path_n
      | None -> false
    end
    (* don't we don't add formula if we haven't yet reached the target because the formulas already exist in the path tree *)
  then x
  else { x with stack = Node_stack.add_formula x.stack @@ lazy_expr () }

let hit_branch (branch : Branch.Runtime.t) (x : t) : t =
  let after_incr = 
    { x with depth_tracker = Depth_tracker.incr_branch x.depth_tracker 
    ; latest_branch = Option.return @@ Branch.Runtime.to_ast_branch branch }
  in
  if after_incr.depth_tracker.is_max_depth
  then after_incr
  else { after_incr with stack = Node_stack.push after_incr.stack branch }

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
  { x with rev_inputs = { clause_id = Concolic_key.id key ; input_value = n } :: x.rev_inputs }
  |> Fn.flip add_lazy_formula @@ fun () -> 
    let Ident s = Concolic_key.id key in
    Dj_common.Log.Export.CLog.app (fun m -> m "Feed %d to %s \n" n s);
    Concolic_riddler.if_pattern key Jayil.Ast.Int_pattern

let add_not (key1 : Concolic_key.t) (key2 : Concolic_key.t) (x : t) : t =
  add_lazy_formula x @@ fun () -> Concolic_riddler.not_ key1 key2

let add_match (k : Concolic_key.t) (m : Concolic_key.t) (pat : Jayil.Ast.pattern) (x : t) : t =
  add_lazy_formula x
  @@ fun () -> Concolic_riddler.match_ k m pat

(*
  -----------------
  BETWEEN-RUN LOGIC   
  -----------------
*)

module Dead =
  struct
    type t =
      { tree          : Root.t
      ; targets       : Target.t list
      ; prev          : T.t }

    let of_sym_session (s : T.t) (root : Root.t) : t =
      (* logically sound to have hit target if formulas are consistent with JIL program *)
      let tree, targets = Node_stack.merge_with_tree s.stack root in
      assert (Option.is_none s.consts.target || Target.is_hit (Option.value_exn s.consts.target) tree); (* check that target was hit in new tree *)
      { tree
      ; targets
      ; prev = s }

    let root (x : t) : Root.t =
      x.tree

    let targets (x : t) : Target.t list =
      x.targets

    let get_status (x : t) : Status.t =
      match x.prev.status with
      | `In_progress ->
          let dt = x.prev.depth_tracker in
          Finished_interpretation { pruned = dt.is_max_depth || dt.is_max_step }
      | `Found_abort branch -> Found_abort (branch, List.rev x.prev.rev_inputs)
      | `Type_mismatch -> Type_mismatch (List.rev x.prev.rev_inputs)

    let is_reach_max_step (x : t) : bool =
      x.prev.depth_tracker.is_max_step
  end

(* Note that other side of all new targets are all the new hits *)
let[@landmarks] finish (x : t) (tree : Root.t) : Dead.t =
  Dead.of_sym_session x tree

let make (root : Root.t) (target : Target.t) (input_feeder : Concolic_feeder.t): t =
  { empty with stack = Node_stack.of_root root ; consts = { empty.consts with target = Some target ; input_feeder } }
