open Core

module Formula_set :
  sig
    type t [@@deriving compare]
    val empty : t
    (* val singleton : Z3.Expr.expr -> t *)
    val add : t -> Z3.Expr.expr -> t
    (* val add_multi : t -> Z3.Expr.expr list -> t *)
    val union : t -> t -> t
    val to_list : t -> Z3.Expr.expr list
    (* val and_ : t -> Z3.Expr.expr *)
  end
  =
  struct
    module Z3_expr =
      struct
        include Z3.Expr
        type t = Z3.Expr.expr

        (* Set.Make expects sexp conversions, but we don't ever use them. *)
        let t_of_sexp _ = failwith "fail t_of_sexp z3 expr"
        let sexp_of_t _ = failwith "fail sexp_of_t x3 expr" 
      end

    module S = Set.Make (Z3_expr)

    type t = S.t [@@deriving compare]

    let empty = S.empty
    (* let singleton = S.singleton *)
    let add = Set.add
    (* let add_multi (s : t) = List.fold ~init:s ~f:add *)
    let union = Set.union
    let to_list = Set.to_list

    (* let and_ (fset : t) : Z3_expr.t =
      match Set.to_list fset with
      | [] -> Riddler.true_
      | exp :: [] -> exp
      | exps -> Riddler.and_ exps *)
  end

module rec Node_base : (* serves as root node *)
  sig
    type t =
      { formulas : Formula_set.t
      ; children : Children.t } [@@deriving compare]
    (** [t] is the root of the JIL program. *)
    val empty : t
    (** [empty] is the tree before the program has ever been run. It has no formulas or children. *)
    (* val add_child : t -> Branch.Runtime.t -> t *)
    val merge : t -> t -> t
    (** [merge a b] combines the trees [a] and [b] and throws an exception if there is a discrepancy. *)
    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formulas t expr] is [t] that has gained [expr] as a formula. *)
    val get_child : t -> Branch.Runtime.t -> Node.t option
    (** [get_child t branch] is the child of [t] by taking the [branch], if it exists. *)
    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target from [t]. *)
    val with_formulas : t -> Formula_set.t -> t
    (** [with_formulas t formulas] is [t] with the given [formulas] overwriting its old formulas. *)
  end
  =
  struct
    type t =
      { formulas : Formula_set.t
      ; children : Children.t } [@@deriving compare]

    let empty : t =
      { formulas = Formula_set.empty
      ; children = Children.empty }

    (* let add_child (x : t) (branch : Branch.Runtime.t) : t =
      { x with children = Children.add_child x.children branch } *)

    let merge (a : t) (b : t) : t =
      { formulas = Formula_set.union a.formulas b.formulas (* TODO: assert they are equal *) (* can also use list here *)
      ; children = Children.merge a.children b.children }

    let add_formula (x : t) (expr : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add x.formulas expr }

    let get_child (x : t) (branch : Branch.Runtime.t) : Node.t option =
      Children.get_child x.children branch

    let is_valid_target (x : t) (branch : Branch.Runtime.t) : bool =
      Children.is_valid_target x.children branch

    let with_formulas (x : t) (formulas : Formula_set.t) : t =
      { x with formulas }
  end
and Children :
  sig
    type t [@@deriving compare]
    (** [t] represents the branches underneath some node. *)
    val empty : t
    (** [empty] is no children. *)
    (* val is_empty : t -> bool *)
    (* val of_branch : Branch.Runtime.t -> t *)
    (* val add_child : t -> Branch.Runtime.t -> t *)
    val of_node : Node.t -> t
    (** [of_node] makes a child out of the node. *)
    val merge : t -> t -> t
    (** [merge a b] merges all children in [a] and [b]. *)
    val get_child : t -> Branch.Runtime.t -> Node.t option
    (** [get_child t branch] is the child in [t] by taking the [branch]. *)
    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target. *)
  end
  =
  struct
    type t = 
      | No_children
      | Both of { true_side : Status.t ; false_side : Status.t } [@@deriving compare]
      (* Could have chosen to have only true or only false, but Status.Unsolved takes care of that. *)
    
    let empty : t = No_children
    (* let is_empty (x : t) : bool =
      match x with
      | No_children -> true
      | _ -> false *)

    let of_node ({ this_branch ; base } as node : Node.t) : t =
      let new_node = Status.Hit node in
      match this_branch with
      | { direction = True_direction ; _ } -> Both { true_side = new_node ; false_side = Unsolved }
      | { direction = False_direction ; _ } -> Both { true_side = Unsolved ; false_side = new_node }

    (* let of_branch (branch : Branch.Runtime.t) : t =
      of_node
      @@ Node.of_parent_branch branch *)

    (* let add_child (x : t) (branch : Branch.Runtime.t) : t =
      match x with
      | No_children -> of_branch branch
      | _ -> failwith "unimplemented" *)

    let merge (a : t) (b : t) : t =
      match a, b with
      | No_children, x
      | x, No_children -> x
      | Both a, Both b ->
        Both
        { true_side = Status.merge a.true_side b.true_side
        ; false_side = Status.merge a.false_side b.false_side }

    let get_child (x : t) (branch : Branch.Runtime.t) : Node.t option =
      match x, branch.direction with
      | Both { true_side = Hit node ; _ }, Branch.Direction.True_direction
      | Both { false_side = Hit node ; _ }, Branch.Direction.False_direction ->
        if Lookup_key.compare node.this_branch.branch_key branch.branch_key <> 0
        then failwith "invalid branch when getting child.";
        Some node
      | _ -> None

    let is_valid_target (x : t) (branch : Branch.Runtime.t) : bool =
      match x, branch.direction with
      | Both { true_side = status ; _ }, Branch.Direction.True_direction
      | Both { false_side = status ; _ }, Branch.Direction.False_direction -> Status.is_valid_target status
      | No_children, _ -> true (* no children *) 

  end
and Status :
  sig
    type t =
      | Hit of Node.t
      | Unsatisfiable
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
      [@@deriving compare]
    (** [t] is a node during a solve. It has been hit, determined unsatisfiable,
        is not known if hittable or unsatisfiable, or has not been solved or seen yet.
        Unsatisfiable or Unknown nodes are status of the node before they've ever been
        hit during interpretation, so there is no existing node as a payload. *)
      
    val merge : t -> t -> t
    (** [merge a b] keeps the most information from [a] or [b] and merges the nodes if both are [Hit]. *)

    val is_valid_target : t -> bool
  end
  =
  struct
    type t =
      | Hit of Node.t
      | Unsatisfiable
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
      [@@deriving compare]

    (*
      Merge by keeping the most info.
      * It is most information to know that we have hit a node. Merge the nodes if necessary.
      * Next is to have solved and determined unsatisfiable
      * After that is solved by timed out, so unknown
      * After that is completely unsolved, which is no information at all
    *)
    let merge (a : t) (b : t) : t =
      match a, b with
      | Hit n1, Hit n2 -> Hit (Node.merge n1 n2)
      | Hit node, _ | _, Hit node -> Hit node
      | Unsatisfiable, _ | _, Unsatisfiable -> Unsatisfiable
      | Unknown, _ | _, Unknown -> Unknown
      | Unsolved, _ -> Unsolved

    let is_valid_target (x : t) : bool =
      match x with
      | Unsolved -> true
      | _ -> false
  end
and Node :
  sig
    type t =
      { this_branch : Branch.Runtime.t
      ; base : Node_base.t } [@@deriving compare]
    (** [t] is a node in the tree that is reached by taking [this_branch]. It has formulas
        and children as in [base]. *)
    
    val of_parent_branch : Branch.Runtime.t -> t
    (** [of_parent_branch branch] is an empty node with label [this_branch = branch]. *)
    (* val to_children : t -> Children.t *)
    (** [to_children t] is the children in [t]. *)
    val merge : t -> t -> t
    (** [merge a b] merges the nodes at [a] and [b] and merges the subtrees of their children.
        Throws an exception if [a.this_branch] and [b.this_branch] are unequal. *)
    val with_formulas : t -> Formula_set.t -> t
    (** [with_formulas t formulas] is [t] where [t.base] has its formulas overwritten with [formulas]. *)
  end
  =
  struct
    type t =
      { this_branch : Branch.Runtime.t
      ; base : Node_base.t } [@@deriving compare]
    
    let of_parent_branch (this_branch : Branch.Runtime.t) : t =
      { this_branch ; base = Node_base.empty }

    (* let to_children (node : t) : Children.t =
      Children.of_node node *)

    let merge (a : t) (b : t) : t =
      if Branch.Runtime.compare a.this_branch b.this_branch <> 0 
      then failwith "trying to merge nodes of a different branch"
      else { this_branch = a.this_branch ; base = Node_base.merge a.base b.base }

    let with_formulas (x : t) (formulas : Formula_set.t) : t =
      { x with base = Node_base.with_formulas x.base formulas }

  end

(* This is just for better naming *)
module Root = Node_base

module Target =
  struct
    (* Notice that this is the same as a node. I wonder if I should have a Target status *)
    type t =
      { branch : Branch.Runtime.t 
      ; from : Node_base.t [@compare.ignore] } [@@deriving compare]

    let of_branch_node (branch : Branch.Runtime.t) (from : Node_base.t) : t =
      { branch ; from }

    let to_formulas ({ branch ; from } : t) : Z3.Expr.expr list =
      Branch.Runtime.to_expr branch :: Formula_set.to_list from.formulas
  end

(* TODO: use the node stack to trace the path to a target in order to update in tree *)
module Node_stack :
  sig
    type t
    (** [t] is a nonempty stack of nodes where the bottom of the stack is a root node. *)
    val empty : t
    (** [empty] has an empty root. *)
    val of_root : Root.t -> t
    (** [of_root root] has the formulas of [root] on the bottom of the stack, but the children are discarded *)
    (* val hd_base : t -> Node_base.t *)
    (** [hd_base t] is the top node base in [t]. *)
    (* val map_hd : t -> f:(Node_base.t -> Node_base.t) -> t *)
    (** [map_hd t ~f] maps the head node base of [t] using [f]. *)
    val merge_with_tree : t -> Root.t -> Root.t * Target.t list
    (** [merge_with_tree t root] creates a tree from the stack [t] and merges with the tree given by [root].
        Also returns a new list of targets from nodes in the tree, where the most prioritized target is at the front. *)
    val push : t -> Branch.Runtime.t -> t
    (** [push t branch] pushes the [branch] onto the stack [t] with a copy of all the formulas already on the stack. *)
    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formula t expr] is [t] where the top node on the stack has gained the formula [expr]. *)
  end
  =
  struct
    type t =
      | Last of Root.t
      | Cons of Node.t * t 

    let empty : t = Last Root.empty

    (* To avoid extra children and to keep the stack a single path, only keep the formulas (and discard the children) from root. *)
    let of_root (root : Root.t) : t =
      Last (Root.with_formulas Root.empty root.formulas)

    let hd_base : t -> Root.t = function
      | Last base
      | Cons ({ base ; _ }, _) -> base

    let map_hd (stack : t) ~(f : Node_base.t -> Node_base.t) : t =
      match stack with
      | Last base -> Last (f base)
      | Cons (hd, tl) -> Cons ({ hd with base = f hd.base }, tl)

    let to_tree (stack : t) : Root.t =
      let rec to_tree acc = function
        | Last root -> { root with children = acc }
        | Cons ({ this_branch ; base }, tl) ->
          let acc = Children.of_node { this_branch ; base = { base with children = acc } } in
          to_tree acc tl
      in
      to_tree Children.empty stack

    (*
      A target is the other direction of a hit branch. This function returns a list
      of all valid targets found in the stack. The most recently hit branches are at the top
      of the returned stack.
      Assumes the stack has already been merged with the tree.
    *)
    let get_targets (stack : t) (tree : Root.t) : Target.t list =
      (* Step from the current node down the branch and adds a new target if possible. Returns new target list and new node *)
      let step (cur_targets : Target.t list) (cur_node : Node_base.t) (branch : Branch.Runtime.t) =
        let other_dir = Branch.Runtime.other_direction branch in
        let next_node = Node_base.get_child cur_node branch |> Option.value_exn in (* logically must exist because is in stack *)
        if Node_base.is_valid_target cur_node other_dir
        then Target.of_branch_node other_dir cur_node :: cur_targets, next_node.base
        else cur_targets, next_node.base
      in
      let rec stack_to_rev_list_no_root acc = function
        | Last _ -> acc (* ignore root *)
        | Cons (node, tl) -> stack_to_rev_list_no_root (node :: acc) tl
      in
      List.fold
        (stack_to_rev_list_no_root [] stack)
        ~init:([], tree)
        ~f:(fun (target_list, cur_node) next_node -> step target_list cur_node next_node.this_branch )
      |> Tuple2.get1

    (* Note that merging two trees would have to visit every node in both of them in the worst case,
       but we know that the tree made from the stack is a single path, so it only has to merge down
       that single path because only one child is hit and needs to be merged (see Status.merge). *)
    let merge_with_tree (stack : t) (tree : Root.t) : Root.t * Target.t list =
      let merged =
        Root.merge tree
        @@ to_tree stack
      in
      merged, get_targets stack merged

    let push (stack : t) (branch : Branch.Runtime.t) : t =
      let formulas = (hd_base stack).formulas in
      Cons (Node.with_formulas (Node.of_parent_branch branch) formulas, stack)

    let add_formula (stack : t) (expr : Z3.Expr.expr) : t =
      map_hd stack ~f:(fun node_base -> Node_base.add_formula node_base expr)
  end

(*
  Runtime is a modifier on Path_tracker, so this is a "Runtime Path Tracker".
  The purpose is to separate the variables that can change when the program is
  interpreted from the variables that only change between interpretations.
*)
module Runtime =
  struct
    module Branch_set = Set.Make (Branch)

    type t =
      { stack          : Node_stack.t
      ; target         : Target.t option
      ; has_hit_target : bool
      ; hit_branches   : Branch_set.t }

    let empty : t =
      { stack          = Node_stack.empty
      ; target         = None
      ; has_hit_target = false
      ; hit_branches   = Branch_set.empty }

    let add_formula (x : t) (expr : Z3.Expr.expr) : t =
      { x with stack = Node_stack.add_formula x.stack expr }

    let hit_branch (x : t) (branch : Branch.Runtime.t) : t =
      (* Format.printf "Hitting branch %s\n" (Branch.Runtime.to_string branch); *)
      { x with stack = Node_stack.add_formula (Node_stack.push x.stack branch) @@ Branch.Runtime.to_expr branch
      ; has_hit_target =
        x.has_hit_target
        || (Option.map x.target ~f:(fun { branch = target_branch ; _ } -> Branch.Runtime.compare branch target_branch = 0) |> Option.value ~default:false)
      ; hit_branches = Set.add x.hit_branches @@ Branch.Runtime.to_ast_branch branch }

    (* Note that other side of all new targets are all the new hits *)
    let finish (x : t) (tree : Root.t ) : Root.t * Target.t list * Branch.t list =
      if Option.is_some x.target && not x.has_hit_target
      then failwith "missed target branch";
      let root, targets = Node_stack.merge_with_tree x.stack tree in
      root, targets, Set.to_list x.hit_branches

    let next (root : Root.t) (target : Target.t) : t =
      { stack          = Node_stack.of_root root
      ; target         = Some target
      ; has_hit_target = false
      ; hit_branches   = Branch_set.empty }
  end

module Target_queue :
  sig
    type t
    (** [t] is a functional priority queue of targets where pushing a target gives
        it the most priority. If the target was already in the queue, the target is
        moved to the front. *)

    val empty : t
    (* val is_empty : t -> bool *)
    (* val push_one : t -> Target.t -> t *)
    val push_list : t -> Target.t list -> t
    (** [push_list t ls] pushes all targets in [ls] onto [t], where earlier items in [ls] have the best priority. *)
    val pop : t -> (Target.t * t) option
    (** [pop t] is most prioritized target and new queue, or [None]. *)
    (* val to_string : t -> string *)

  end
  =
  struct
    (* Functional queue of target with priority Int *)
    (* Maps key (target) to priority (int) and allows quick access to best (ie min) priority. *)
    module Q = Psq.Make (Target) (Int)
    type t = Q.t

    let empty : t = Q.empty
    (* let is_empty : t -> bool = Q.is_empty *)

    (* default priority for least prioritized element in the queue. *)
    let default_prio = 0

    (* let push_one (queue : t) (target : Target.t) : t =
      match Q.min queue with (* O(1) access of most prioritized *) 
      | None -> Q.push target default_prio queue (* queue was empty *)
      | Some (_, best_prio) -> Q.push target (best_prio - 1) queue  (* push target with best priority *) *)

    (* For more efficiency, can just get the best priority once and add manually without `push_one` *)
    let push_list (queue : t) (ls : Target.t list) : t =
      let n = List.length ls in
      let old_best_prio =
        match Q.min queue with
        | Some (_, best_prio) -> best_prio
        | None -> default_prio
      in
      let new_best_prio = old_best_prio - n in
      let new_queue =
        ls
        |> List.mapi ~f:(fun i target -> target, i + new_best_prio)
        |> Q.of_list
      in
      if Q.is_empty queue
      then new_queue
      else Q.(queue ++ new_queue) (* merge the two queues *)

    let pop (queue : t) : (Target.t * t) option =
      match Q.pop queue with
      | None -> None
      | Some ((target, _), q) -> Some (target, q)

    let to_string (queue : t) : string =
      queue
      |> Q.to_priority_list
      |> List.to_string ~f:(fun (target, i) -> let open Target in Format.sprintf "(target:%s, priority:%d)\n" (Branch.Runtime.to_string target.branch) i)
  end

(*
  The user will keep a [t] and use it to enter branches. When the interpretation finishes,
  they will say so, and the stack is traversed to be included in total.   

  This way, we are not modifying the entire path in the tree with every step, and also we 
  don't have to use mutation to avoid it. It just takes one extra pass through the whole thing
  at the end.

  [t] will manage between-run and during-run logic, so the only only has to interface with
  this [t]. The during-run logic is abstracted into Runtime above.

  TODO: solving for target and connection to session
*)
type t =
  { tree         : Root.t (* pointer to the root of the entire tree of paths *)
  ; target_queue : Target_queue.t
  ; runtime      : Runtime.t
  ; branches     : Branch_tracker.Status_store.Without_payload.t (* quick patch using status store from loose concolic evaluator *)
  ; run_num      : int }

let empty : t =
  { tree         = Root.empty
  ; target_queue = Target_queue.empty
  ; runtime      = Runtime.empty
  ; branches     = Branch_tracker.Status_store.Without_payload.empty
  ; run_num      = 0 }

let of_expr (expr : Jayil.Ast.expr) : t =
  { empty with branches = Branch_tracker.Status_store.Without_payload.of_expr expr }

module Formula_logic =
  struct
    let add_formula (x : t) (expr : Z3.Expr.expr) : t =
      { x with runtime = Runtime.add_formula x.runtime expr }

    let add_key_eq_val (x : t) (key : Lookup_key.t) (v : Jayil.Ast.value) : t =
      add_formula x @@ Riddler.eq_term_v key (Some v)

    let add_alias (x : t) (key1 : Lookup_key.t) (key2 : Lookup_key.t) : t =
      add_formula x @@ Riddler.eq key1 key2

    let add_binop (x : t) (key : Lookup_key.t) (op : Jayil.Ast.binary_operator) (left : Lookup_key.t) (right : Lookup_key.t) : t =
      add_formula x @@ Riddler.binop_without_picked key op left right

    let add_input (x : t) (key : Lookup_key.t) (v : Dvalue.t) : t =
      let Ident s = key.x in
      let n =
        match v with
        | Dvalue.Direct (Value_int n) -> n
        | _ -> failwith "non-int input" (* logically impossible *)
      in
      if Printer.print then Format.printf "Feed %d to %s \n" n s;
      add_formula x @@ Riddler.is_pattern key Jayil.Ast.Int_pattern

    let hit_branch (x : t) (branch : Branch.Runtime.t) : t =
      (* Format.printf "hit branch %s\n" (Branch.Runtime.to_string branch); *)
      { x with runtime = Runtime.hit_branch x.runtime branch }
  end

include Formula_logic

let default_global_max_step = Int.(2 * 10 ** 3)

(* TODO: delete this because it's really bad when run with tests instead of a single file *)
let unsat_count = Hashtbl.create (module Branch.Runtime)

let next (x : t) : [ `Done of Branch_tracker.Status_store.Without_payload.t | `Next of (t * Session.Eval.t) ] =
  (* first finish*)
  let updated_tree, new_targets, hit_branches = Runtime.finish x.runtime x.tree in
  (* Format.printf "hit branches = %s\n" (List.to_string hit_branches ~f:Branch.to_string); *)
  let updated_branches =
    List.fold
      hit_branches
      ~init:x.branches
      ~f:(Branch_tracker.Status_store.Without_payload.set_branch_status ~new_status:Hit)
  in
  let rec next (x : t) : [ `Done of Branch_tracker.Status_store.Without_payload.t | `Next of (t * Session.Eval.t) ] =
    (* Format.printf "In `next`. Queue is %s.\n" (Target_queue.to_string x.target_queue); *)
    match Target_queue.pop x.target_queue with
    | Some (target, target_queue) -> 
      solve_for_target { x with target_queue } target
    | None when x.run_num = 0 ->
      `Next ({ x with run_num = 1 }, Session.Eval.create Concolic_feeder.default default_global_max_step)
    | None -> (* no targets left, so done *)
      `Done x.branches
  and solve_for_target (x : t) (target : Target.t) =
    let new_solver = Z3.Solver.mk_solver Solver.SuduZ3.ctx None in
    Z3.Solver.add new_solver (Target.to_formulas target);
    (* Format.printf "%s\n" (Z3.Solver.to_string new_solver); *)
    (* Format.printf "Solving for target %s\n" (Branch.Runtime.to_string target.branch); *)
    match Z3.Solver.check new_solver [] with
    | Z3.Solver.UNSATISFIABLE ->
      Format.printf "FOUND UNSATISFIABLE\n";
      Hashtbl.update unsat_count target.branch ~f:(function None -> 1 | Some n -> Format.printf "New unsat found %d\n" (n + 1); n + 1);
      next x (* TODO: update in tree. We don't do this, so sometimes gets solved again *)
    | Z3.Solver.UNKNOWN -> Format.printf "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n"; next x
    | Z3.Solver.SATISFIABLE ->
      `Next (
        { x with runtime = Runtime.next x.tree target ; run_num = x.run_num + 1 }
        , Z3.Solver.get_model new_solver
          |> Core.Option.value_exn
          |> Concolic_feeder.from_model
          |> fun feeder -> Session.Eval.create feeder default_global_max_step
      )
  in
  { x with tree = updated_tree
  ; target_queue = Target_queue.push_list x.target_queue new_targets
  ; branches = updated_branches
  }
  |> next 


let status_store ({ branches ; _ } : t) : Branch_tracker.Status_store.Without_payload.t =
  branches