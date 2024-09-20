
open Core

module type NODE =
  sig
    type children

    type t =
      { formulas : Formula_set.t
      ; children : children }

    val empty : t

    val formulas_of_target : t -> Target.t -> Z3.Expr.expr list

    val of_stem : Formulated_stem.t -> bool -> t * Target.t list

    val add_stem : t -> Target.t -> Formulated_stem.t -> bool -> t * Target.t list
    (** [add_stem t old_target stem failed_assume] adds the [stem] to the path tree [t] beginning from the
        [old_target], which was hit at the root of the stem. The interpretation that generated the [stem]
        ended in a failed assume/assert iff [failed_assume] is true.
          
        The new path tree and the acquired targets are returned. *)

    val set_unsat_target : t -> Target.t -> t
    (** [set_unsat_target t target] is [t] where the given [target] has been marked off as unsatisfiable. *)
  end

module type CHILDREN =
  sig
    type child
    type node

    type t =
      | Pruned (* to signify end of tree in any way. We prune at max depth and when both children are collapsed *)
      | Both of { true_side : child ; false_side : child }

    val is_empty : t -> bool

    val child_exn : t -> Branch.Direction.t -> child

    val update : t -> Branch.Direction.t -> child -> t

    val make_failed_assume : Branch.Runtime.t -> Formula_set.t -> Path.t -> t * Target.t * Target.t

    val of_branch : Branch.Runtime.t -> node -> Path.t -> t * Target.t
  end

module type CHILD =
  sig
    type node

    type t =
      | Hit of node 
      | Target_acquired of { constraints : Formula_set.t }
      | Waiting_to_pass_assume of { assumed_formulas : Formula_set.t } (* can join with target acquired... *)
      | Unsatisfiable
      | Solver_timeout

    val node_exn : t -> node
    val make_hit_node : node -> t
    val make_failed_assume_child : Formula_set.t -> t
    val make_target_child : Branch.Runtime.t -> t
  end

module rec Node :
  NODE with
  type children := Children.t
  =
  struct
    type t =
      { formulas : Formula_set.t
      ; children : Children.t }

    let empty : t =
      { formulas = Formula_set.empty
      ; children = Children.Pruned }

    let is_empty (x : t) : bool =
      Formula_set.equal x.formulas Formula_set.empty
      && Children.is_empty x.children

    let child_node_exn (x : t) (dir : Branch.Direction.t) : t =
      Child.node_exn
      @@ Children.child_exn x.children dir

    let formulas_of_target (tree : t) (target : Target.t) : Z3.Expr.expr list =
      let rec trace_path acc parent = function
        | last_dir :: [] -> begin
          match Children.child_exn parent.children last_dir with
          | Target_acquired { constraints }
          | Waiting_to_pass_assume { assumed_formulas = constraints } ->
            Formula_set.to_list constraints @ Formula_set.to_list parent.formulas @ acc
          | _ -> failwith "target not at end of path"
        end
        | next_dir :: tl ->
          trace_path
            (Formula_set.to_list parent.formulas @ acc)
            (child_node_exn parent next_dir)
            tl
        | [] -> failwith "no path given for target"
      in
      trace_path [] tree target.path.forward_path

    (*
      The way we handle paths here is very inefficient. A reverse path would be smart

      TODO: see about how we handle failing an assume immediately in root of the stem (because the root of the stem might not be the global scope)
    *)
    let node_of_stem (initial_path : Path.t) (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      (* path passed in here is the path that includes the branch in the cons, or is empty if root *)
      let rec make_node path acc_children acc_targets stem =
        match stem with
        | Formulated_stem.Root { root_formulas } ->
          { formulas = root_formulas ; children = acc_children }, acc_targets
        | Cons { branch ; formulas ; tail } ->
          let path_to_children = Path.drop_last_exn path in (* drop the branch off the path *)
          let new_children, new_target = Children.of_branch branch { formulas ; children = acc_children } path_to_children in
          make_node path_to_children new_children (new_target :: acc_targets) tail
      in
      let full_path = Path.concat initial_path @@ Formulated_stem.to_path stem in
      match stem with
      | Cons { branch ; formulas ; tail } when failed_assume ->
        let path_to_assume = Path.drop_last_exn full_path in
        let children, t1, t2 = Children.make_failed_assume branch formulas path_to_assume in
        make_node path_to_assume children [ t1 ; t2 ] tail
      | Cons _ ->
        make_node full_path Pruned [] stem
      | Root { root_formulas } ->
        { formulas = root_formulas ; children = Pruned }, []

    let of_stem (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      node_of_stem Path.empty stem failed_assume

    let add_stem (tree : t) (target : Target.t) (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      if is_empty tree
      then node_of_stem Path.empty stem failed_assume
      else
        let rec loop path parent finish =
          match path with
          | [] -> failwith "setting target with no path"
          | last_dir :: [] -> (* would step onto target node if we followed last_dir *)
            let new_node, targets = node_of_stem target.path stem failed_assume in
            finish ({ parent with children = Children.update parent.children last_dir @@ Child.make_hit_node new_node }, targets)
          | next_dir :: tl ->
            loop tl (child_node_exn parent next_dir) (fun (child_node, targets) ->
              finish ({ parent with children = Children.update parent.children next_dir @@ Child.make_hit_node child_node }, targets)
            )
        in
        loop target.path.forward_path tree (fun a -> a)

    (*
      No pruning yet. Just update tree and leave it hanging out there in memory
    *)
    let set_unsat_target (tree : t) (target : Target.t) : t =
      let rec loop path parent finish =
        match path with
        | [] -> failwith "setting target with no path"
        | last_dir :: [] -> finish { parent with children = Children.update parent.children last_dir Child.Unsatisfiable }
        | next_dir :: tl ->
          loop tl (child_node_exn parent next_dir) (fun node ->
            finish { parent with children = Children.update parent.children next_dir @@ Child.make_hit_node node }
          )
      in
      loop target.path.forward_path tree (fun a -> a)
          
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

    let is_empty (x : t) : bool =
      match x with
      | Pruned -> true
      | Both _ -> false

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

    let make_failed_assume (branch : Branch.Runtime.t) (assumed_formulas : Formula_set.t) (path : Path.t) : t * Target.t * Target.t =
      let failed_assume_child = Child.make_failed_assume_child assumed_formulas in
      let failed_assume_target = Target.create branch @@ Path.append path branch.direction in

      let branch_other_dir = Branch.Runtime.other_direction branch in
      let other_child = Child.make_target_child branch_other_dir in
      let other_target = Target.create branch_other_dir @@ Path.append path branch_other_dir.direction in

      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = failed_assume_child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = failed_assume_child }
      in
      children, failed_assume_target, other_target

    let of_branch (branch : Branch.Runtime.t) (node : Node.t) (path : Path.t) : t * Target.t =
      let branch_other_dir = Branch.Runtime.other_direction branch in
      let child = Child.make_hit_node node in
      let other_child = Child.make_target_child branch_other_dir in
      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = child }
      in
      children, Target.create branch_other_dir @@ Path.append path branch_other_dir.direction
  end
and Child :
  CHILD with
  type node := Node.t
  =
  struct
    type t =
      | Hit of Node.t 
      | Target_acquired of { constraints : Formula_set.t }
      | Waiting_to_pass_assume of { assumed_formulas : Formula_set.t }
      | Unsatisfiable
      | Solver_timeout

    let node_exn (x : t) : Node.t =
      match x with
      | Hit node -> node
      | _ -> failwith "no node in node_exn"

    let make_hit_node (node : Node.t) : t =
      Hit node

    let make_failed_assume_child (assumed_formulas : Formula_set.t) : t =
      Waiting_to_pass_assume { assumed_formulas } 

    let make_target_child (branch : Branch.Runtime.t) : t =
      Target_acquired { constraints = Formula_set.singleton @@ Branch.Runtime.to_expr branch }

  end

type t =
  { root         : Node.t
  ; target_queue : Target_queue.t }

(* let empty : t =
  { root         = Node.empty
  ; target_queue = Target_queue.empty } *)

let of_options : (unit, t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (() : unit) ->
    { root = Node.empty ; target_queue = Options.Fun.run Target_queue.of_options r () }

let formulas_of_target (x : t) (target : Target.t) : Z3.Expr.expr list =
  Node.formulas_of_target x.root target

let enqueue (x : t) (targets : Target.t list) : t =
  { x with target_queue = Target_queue.push_list x.target_queue targets }

let enqueue_result (x : t) (g : unit -> Node.t * Target.t list) : t =
  let root, targets = g () in
  enqueue { x with root } targets

let of_stem : (Formulated_stem.t, bool -> Branch.t list -> t) Options.Fun.t =
  Options.Fun.make
  @@ fun (r : Options.t) -> fun (stem : Formulated_stem.t) -> fun (failed_assume : bool) -> fun (hit_branches : Branch.t list) ->
    let x = Options.Fun.run of_options r () in
    let x = { x with target_queue = Target_queue.hit_branches x.target_queue hit_branches } in
    enqueue_result x (fun _ -> Node.of_stem stem failed_assume)

let add_stem (x : t) (target : Target.t) (stem : Formulated_stem.t) (failed_assume : bool) (hit_branches : Branch.t list) : t =
  let x = { x with target_queue = Target_queue.hit_branches x.target_queue hit_branches } in (* TODO: fix this ugly quick patch, and above in of_stem *)
  enqueue_result x (fun _ -> Node.add_stem x.root target stem failed_assume)

let set_unsat_target (x : t) (target : Target.t) : t =
  { x with root = Node.set_unsat_target x.root target }

let pop_target ?(kind : Target_queue.Pop_kind.t = DFS) (x : t) : (Target.t * t) option =
  Option.map ~f:(fun (target, new_queue) -> target, { x with target_queue = new_queue })
  @@ Target_queue.pop ~kind x.target_queue
