
open Core
open Options.Fun.Infix


(*
  NOTES:
    I want to store the caches at each node level, and that way when
    I go to create a symbolic session, I can just copy the cache from
    that target and not have to recreate the cache.

    The only expressions I need to load are those for the branches.
    Right now, I create a big expression for each branch without extracting
    out subexpressions. I should like later to extract out any subexpression
    that is used more than once.
    However, temporarily, the expression cache maps a key to the only expression
    it needs, and it can be equated blindly with that expression.
    
    The cache of each node is contained in the caches of its children. It only
    exists for easy copying. I should note that I can discard the cache once
    neither child is a target because it will never be needed again. This is a TODO.

  TODO: don't only load in the claim for the branch but also the expression for that
    key. It is not enough to just claim the condition key takes the direction, but I 
    also need to load in everything that the condition key is supposed to be.
*)

module type NODE =
  sig
    type children

    type t =
      { expr_cache : Expression.Cache.t
      ; children   : children }

    val empty : t
    val claims_of_target : t -> Target.t -> Claim.t list * Expression.Cache.t
    val of_stem : Formulated_stem.t -> bool -> t * Target.t list
    val add_stem : t -> Target.t -> Formulated_stem.t -> bool -> t * Target.t list
    (** [add_stem t old_target stem failed_assume] adds the [stem] to the path tree [t] beginning from the
        [old_target], which was hit at the root of the stem. The interpretation that generated the [stem]
        ended in a failed assume/assert iff [failed_assume] is true.
          
        The new path tree and the acquired targets are returned. *)

    val set_unsat_target : t -> Target.t -> t
    (** [set_unsat_target t target] is [t] where the given [target] has been marked off as unsatisfiable. *)

    val set_timeout_target : t -> Target.t -> t
    (** [set_timeout_target t target] is [t] where the given [target] has been marked off as causing a solver timeout. *)
  end

module type CHILDREN =
  sig
    type child
    type node

    type t =
      | Pruned (* to signify end of tree in any way. We prune at max depth and when both children are collapsed *)
      | Both of { true_side : child ; false_side : child }
    (* val is_empty : t -> bool *)
    val child_exn : t -> Branch.Direction.t -> child
    val update : t -> Branch.Direction.t -> child -> t
    (* val make_failed_assume : Branch.Runtime.t -> Formula_set.t -> Path.Reverse.t -> t * Target.t * Target.t *)
    val of_branch : Branch.Runtime.t -> node -> Path.Reverse.t -> t * Target.t
  end

module type CHILD =
  sig
    type node

    type t =
      | Hit of { node : node ; constraint_ : Claim.t }
      | Target_acquired of { constraint_ : Claim.t }
      (*| Waiting_to_pass_assume of { assumed_formulas : Formula_set.t }*) (* can join with target acquired... *)
      | Unsatisfiable
      | Solver_timeout

    val node_and_claim_exn : t -> node * Claim.t
    val make_hit_node : node -> Branch.Runtime.t -> t
    (* val make_failed_assume_child : Formula_set.t -> t *)
    val make_target_child : Branch.Runtime.t -> t
  end

module rec Node :
  NODE with
  type children := Children.t
  =
  struct
    type t =
      { expr_cache : Expression.Cache.t
      ; children   : Children.t }

    let empty : t =
      { expr_cache = Expression.Cache.empty
      ; children   = Children.Pruned }

    let child_node_exn (x : t) (dir : Branch.Direction.t) : t * Claim.t =
      Child.node_and_claim_exn
      @@ Children.child_exn x.children dir

    let claims_of_target (tree : t) (target : Target.t) : Claim.t list * Expression.Cache.t =
      let rec trace_path acc parent = function
        | last_dir :: [] -> begin
          match Children.child_exn parent.children last_dir with
          | Target_acquired { constraint_ } -> constraint_ :: acc, parent.expr_cache
          (* | Waiting_to_pass_assume { assumed_formulas = constraints } ->
            Formula_set.to_list constraints @ Formula_set.to_list parent.formulas @ acc *)
          | _ -> failwith "target not at end of path"
        end
        | next_dir :: tl ->
          let node_exn, claim = child_node_exn parent next_dir in
          trace_path
            (claim :: acc)
            node_exn
            tl
        | [] -> failwith "no path given for target"
      in
      trace_path [] tree (Path.Reverse.to_forward_path target.path).forward_path

    (*
      TODO: see about how we handle failing an assume immediately in root of the stem (because the root of the stem might not be the global scope)
    *)
    let node_of_stem (initial_path : Path.Reverse.t) (stem : Formulated_stem.t) (_failed_assume : bool) : t * Target.t list =
      (* path passed in here is the path that includes the branch in the cons, or is empty if root *)
      let rec make_node path acc_children stem acc_targets =
        match stem with
        | Formulated_stem.Root { expr_cache } ->
          { expr_cache ; children = acc_children }, acc_targets
        | Cons { branch ; expr_cache ; tail } ->
          let path_to_children = Path.Reverse.drop_hd_exn path in (* drop the branch off the path *)
          let new_children, new_target = Children.of_branch branch { expr_cache ; children = acc_children } path_to_children in
          make_node path_to_children new_children tail (new_target :: acc_targets)
      in
      let full_path = Path.Reverse.concat (Formulated_stem.to_rev_path stem) initial_path in
      match stem with
      (* | Cons { branch ; formulas ; tail } when failed_assume -> (* TODO: think about failed assume *)
        let path_to_assume = Path.Reverse.drop_hd_exn full_path in
        let children, t1, t2 = Children.make_failed_assume branch formulas path_to_assume in
        make_node path_to_assume children tail [ t1 ; t2 ] *)
      | Cons _ ->
        make_node full_path Pruned stem []
      | Root { expr_cache } ->
        { expr_cache ; children = Pruned }, []

    let of_stem (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      node_of_stem Path.Reverse.empty stem failed_assume

    let add_stem (tree : t) (target : Target.t) (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      let rec loop path parent finish =
        match path with
        | [] -> failwith "setting target with no path"
        | last_dir :: [] -> (* would step onto target node if we followed last_dir *)
          let new_node, targets = node_of_stem target.path stem failed_assume in
          finish ({ parent with children = Children.update parent.children last_dir @@ Child.make_hit_node new_node target.branch }, targets)
        | next_dir :: tl ->
          let next_node, claim = child_node_exn parent next_dir in (* TODO: clean this up *)
          loop tl next_node (fun (node, targets) ->
            finish ({ parent with children = Children.update parent.children next_dir (Child.Hit { node ; constraint_ = claim }) }, targets)
          )
      in
      loop (Path.Reverse.to_forward_path target.path).forward_path tree (fun a -> a)

    (*
      No pruning yet. Just update tree and leave it hanging out there in memory
    *)
    let set_target (tree : t) (target : Target.t) (new_child : Child.t) : t =
      let rec loop path parent finish =
        match path with
        | [] -> failwith "setting target with no path"
        | last_dir :: [] -> finish { parent with children = Children.update parent.children last_dir new_child }
        | next_dir :: tl ->
          let next_node, claim = child_node_exn parent next_dir in (* TODO: clean this up *)
          loop tl next_node (fun node ->
            finish { parent with children = Children.update parent.children next_dir (Child.Hit { node ; constraint_ = claim }) }
          )
      in
      loop (Path.Reverse.to_forward_path target.path).forward_path tree (fun a -> a)
 
    let set_timeout_target (tree : t) (target : Target.t) : t =
      set_target tree target Child.Solver_timeout

    let set_unsat_target (tree : t) (target : Target.t) : t =
      set_target tree target Child.Unsatisfiable
  end
and Children :
  CHILDREN with
  type child := Child.t and
  type node  := Node.t
  =
  struct
    type t =
      | Pruned (* to signify end of tree in any way. We prune at max depth or if children can't exist (or later if they're exhausted) *)
      | Both of { true_side : Child.t ; false_side : Child.t }

    (* let is_empty (x : t) : bool =
      match x with
      | Pruned -> true
      | Both _ -> false *)

    let child_exn (x : t) (dir : Branch.Direction.t) : Child.t =
      match x with
      | Pruned -> failwith "no child in child_exn"
      | Both r ->
        match dir with
        | True_direction -> r.true_side
        | False_direction -> r.false_side

    let update (x : t) (dir : Branch.Direction.t) (child : Child.t) : t =
      match x with
      | Pruned -> failwith "invalid argument"
      | Both r ->
        match dir with
        | True_direction -> Both { r with true_side = child }
        | False_direction -> Both { r with false_side = child }

    (* let make_failed_assume (branch : Branch.Runtime.t) (assumed_formulas : Formula_set.t) (path : Path.Reverse.t) : t * Target.t * Target.t =
      let failed_assume_child = Child.make_failed_assume_child assumed_formulas in

      let branch_other_dir = Branch.Runtime.other_direction branch in
      let other_child = Child.make_target_child branch_other_dir in

      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = failed_assume_child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = failed_assume_child }
      in
      children
      , Target.create branch @@ Path.Reverse.cons branch.direction path
      , Target.create branch_other_dir @@ Path.Reverse.cons branch_other_dir.direction path *)


    let of_branch (branch : Branch.Runtime.t) (node : Node.t) (path : Path.Reverse.t) : t * Target.t =
      let branch_other_dir = Branch.Runtime.other_direction branch in
      let child = Child.make_hit_node node branch in
      let other_child = Child.make_target_child branch_other_dir in

      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = child }
      in
      children
      , Target.create branch_other_dir @@ Path.Reverse.cons branch_other_dir.direction path
  end
and Child :
  CHILD with
  type node := Node.t
  =
  struct
    type t =
      | Hit of { node : Node.t ; constraint_ : Claim.t }
      | Target_acquired of { constraint_ : Claim.t }
      (* | Waiting_to_pass_assume of { assumed_formulas : Formula_set.t } *)
      | Unsatisfiable
      | Solver_timeout

    let node_and_claim_exn (x : t) : Node.t * Claim.t =
      match x with
      | Hit { node ; constraint_ } -> node, constraint_
      | _ -> failwith "no node in node_exn"

    let make_hit_node (node : Node.t) (branch : Branch.Runtime.t) : t =
      Hit { node ; constraint_ = Branch.Runtime.to_claim branch }

    (* let make_failed_assume_child (assumed_formulas : Formula_set.t) : t =
      Waiting_to_pass_assume { assumed_formulas }  *)

    let make_target_child (branch : Branch.Runtime.t) : t =
      Target_acquired { constraint_ = Branch.Runtime.to_claim branch }
  end

type t =
  { root         : Node.t
  ; target_queue : Target_queue.t }

let of_options : (unit, t) Options.Fun.a =
  Target_queue.of_options
  ^>> fun target_queue -> { root = Node.empty ; target_queue }

let claims_of_target (x : t) (target : Target.t) : Claim.t list * Expression.Cache.t =
  Node.claims_of_target x.root target

let cache_of_target (x : t) (target : Target.t) : Expression.Cache.t =
  Tuple2.get2
  @@ Node.claims_of_target x.root target

let enqueue (x : t) (targets : Target.t list) : t =
  { x with target_queue = Target_queue.push_list x.target_queue targets }

let enqueue_result (x : t) (g : unit -> Node.t * Target.t list) : t =
  let root, targets = g () in
  enqueue { x with root } targets

let of_stem : (Formulated_stem.t, bool -> Branch.t list -> t) Options.Fun.a =
  Options.Fun.thaw
  @@ of_options
  ^>> fun x -> fun (stem : Formulated_stem.t) (failed_assume : bool) (hit_branches : Branch.t list) ->
    enqueue_result
      { x with target_queue = Target_queue.hit_branches x.target_queue hit_branches }
      (fun _ -> Node.of_stem stem failed_assume)

let add_stem (x : t) (target : Target.t) (stem : Formulated_stem.t) (failed_assume : bool) (hit_branches : Branch.t list) : t =
  let x = { x with target_queue = Target_queue.hit_branches x.target_queue hit_branches } in (* TODO: fix this ugly quick patch, and above in of_stem *)
  enqueue_result x (fun _ -> Node.add_stem x.root target stem failed_assume)

let set_unsat_target (x : t) (target : Target.t) : t =
  { x with root = Node.set_unsat_target x.root target }

let set_timeout_target (x : t) (target : Target.t) : t =
  { x with root = Node.set_timeout_target x.root target }

let pop_target ?(kind : Target_queue.Pop_kind.t = DFS) (x : t) : (Target.t * t) option =
  Option.map ~f:(fun (target, new_queue) -> target, { x with target_queue = new_queue })
  @@ Target_queue.pop ~kind x.target_queue
