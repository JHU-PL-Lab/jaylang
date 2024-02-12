open Core

open Tree

module rec Target :
  sig
    type t =
      { child : Child.t
      ; path  : Path.t }
      [@@deriving compare]
    val create : Child.t -> Path.t -> t
    val to_formulas : t -> Root.t -> Z3.Expr.expr list
  end
  =
  struct
    type t =
      { child : Child.t
      ; path  : Path.t } (* The path just helps the solver find the node in the tree in order to gather formulas *)
      [@@deriving compare]
      (* We do need to compare path because the child key doesn't include exited branches, but the path does, so path is necessary to describe target completely *)

    let create (child : Child.t) (path : Path.t) : t =
      { child ; path }

    let to_formulas ({ child ; path } : t) (root : Root.t) : Z3.Expr.expr list =
      let target_branch = child.branch in
      let target_key = target_branch.branch_key in
      (* acc already contains all formulas pertaining to `node` *)
      let rec trace_path acc node = function
        | next_branch :: tl -> begin
          match Node.get_child node target_branch with
          | Some target -> (* found the target as a child of node *)
            Child.to_formulas target @ acc
          | None -> (* target is not a child of the node, so continue down the path *)
            let next_child = Node.get_child_exn node next_branch in
            trace_path
              (Child.to_formulas next_child @ acc)
              (Child.to_node_exn next_child)
              tl
          end
        | [] -> Child.to_formulas child @ acc (* necessarily has found the target *)
      in
      trace_path (Formula_set.to_list root.formulas) root path
      
  end
and Node_stack :
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
    (* val to_path : t -> Path.t *)
  end
  =
  struct
    type t =
      | Last of Root.t
      | Cons of Child.t * t 

    let empty : t = Last Root.empty

    (* To avoid extra children and to keep the stack a single path, only keep the formulas (and discard the children) from root. *)
    let of_root (root : Root.t) : t =
      Last (Root.with_formulas Root.empty root.formulas)

    let hd_node : t -> Root.t = function
      | Last node -> node
      | Cons (child, _) -> Child.to_node_exn child

    let map_hd (stack : t) ~(f : Node.t -> Node.t) : t =
      match stack with
      | Last base -> Last (f base)
      | Cons (hd, tl) -> Cons (Child.map_node hd ~f, tl) (* might be a good choice to throw exception if node doesn't exist *)

    let to_tree (stack : t) : Root.t =
      let rec to_tree acc = function
        | Last root -> { root with children = acc }
        | Cons (child, tl) ->
          let acc = Children.set_node Children.empty child.branch { (Child.to_node_exn child) with children = acc } in
          to_tree acc tl
      in
      to_tree Children.empty stack

    let to_path (x : t) : Path.t =
      let rec loop acc = function
        | Last _ -> acc
        | Cons (child, tl) -> loop (child.branch :: acc) tl
      in
      loop [] x

    (*
      A target is the other direction of a hit branch. This function returns a list
      of all valid targets found in the stack. The most recently hit branches are at the top
      of the returned stack.
      Assumes the stack has already been merged with the tree.

      I think the total complexity of this has to be quadratic because we need to cut a path short
      in order for it to stop at the target. This isn't great, and maybe its better to keep this
      shorter by storing a reverse path, and we reverse it when we want to trace it.
    *)
    let get_targets (stack : t) (tree : Root.t) : Target.t list =
      let rec step (cur_targets : Target.t list) (cur_node : Node.t) (prev_path : Path.t) (remaining_path : Path.t) =
        match remaining_path with
        | branch :: tl ->
          let other_dir = Branch.Runtime.other_direction branch in
          let next_node = Node.get_child_exn cur_node branch |> Child.to_node_exn in
          if Node.is_valid_target_child cur_node other_dir
          then
            step
              (Target.create (Node.get_child_exn cur_node other_dir) prev_path :: cur_targets)
              next_node
              (prev_path @ [branch])
              tl
          else
            step
              cur_targets
              next_node
              (prev_path @ [branch])
              tl
        | [] -> cur_targets
      in
      step [] tree [] (to_path stack)
      (* This is the beginning of an attempt to improve time complexity *)
      (* let rec step (cur_node : Node.t) (path : Path.t) : Target.t list =
        match path with
        | branch :: tl -> 
          let next_node = Child.to_node_exn @@ Node.get_child_exn cur_node branch in
          let acc = step next_node tl in (* NOT TAIL RECURSIVE! *)
          let other_dir = Branch.Runtime.other_direction branch in
          if Node.is_valid_target_child cur_node other_dir
          then Target.create (Node.get_child_exn ) *)

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
      Cons (Child.create Node.empty branch, stack)

    let add_formula (stack : t) (expr : Z3.Expr.expr) : t =
      map_hd stack ~f:(fun node -> Node.add_formula node expr)
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
        || (match x.target with None -> false | Some target -> Branch.Runtime.compare branch target.child.branch = 0)
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

    (* let to_string (queue : t) : string =
      queue
      |> Q.to_priority_list
      |> List.to_string ~f:(fun (target, i) -> let open Target in Format.sprintf "(target:%s, priority:%d)\n" (Branch.Runtime.to_string target.child.branch) i) *)
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
(* let unsat_count = Hashtbl.create (module Branch.Runtime) *)

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
    Z3.Solver.add new_solver (Target.to_formulas target x.tree);
    (* Format.printf "%s\n" (Z3.Solver.to_string new_solver); *)
    (* Format.printf "\nSolving for target %s\n" (Branch.Runtime.to_string target.child.branch); *)
    (* Format.printf "Path is%s\n" (List.to_string target.path ~f:(Branch.Runtime.to_string_short)); *)
    match Z3.Solver.check new_solver [] with
    | Z3.Solver.UNSATISFIABLE ->
      Format.printf "FOUND UNSATISFIABLE\n";
      (* Hashtbl.update unsat_count target.child.branch ~f:(function None -> 1 | Some n -> Format.printf "New unsat found %d\n" (n + 1); n + 1); *)
      next { x with tree = Root.set_status x.tree target.child Status.Unsatisfiable target.path }
    | Z3.Solver.UNKNOWN -> Format.printf "FOUND UNKNOWN DUE TO SOLVER TIMEOUT\n";
      next { x with tree = Root.set_status x.tree target.child Status.Unknown target.path }
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