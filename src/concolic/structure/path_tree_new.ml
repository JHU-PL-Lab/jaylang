
open Core

module type NODE =
  sig
    type children

    type t =
      { formulas : Formula_set.t
      ; children : children }

    val empty : t

    val formulas_of_target : t -> Target.t -> Z3.Expr.expr list

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

    val show : t -> unit
    val node_exn : t -> node
    val make_hit_node : node -> t
    val make_failed_assume_child : Branch.Runtime.t -> Formula_set.t -> t
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
      Format.printf "%d\n" (List.length target.path.forward_path);
      let rec trace_path acc node = function
        | last_dir :: [] -> begin
          match Children.child_exn node.children last_dir with
          | Target_acquired { constraints } -> Formula_set.to_list constraints @ acc
          | Waiting_to_pass_assume { assumed_formulas } -> Formula_set.to_list assumed_formulas @ acc
          | child -> Child.show child; Format.printf "%d\n" (List.length target.path.forward_path); failwith "target not at end of path"
        end
        | next_dir :: tl ->
          let next_node = child_node_exn node next_dir in
          trace_path
            (Formula_set.to_list next_node.formulas @ acc)
            next_node
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
        Format.printf "stem is just root\n";
        { formulas = root_formulas ; children = Pruned }, []

    (*
      For now, non-tail-recursively add the stem.
    *)
    let add_stem (tree : t) (target : Target.t) (stem : Formulated_stem.t) (failed_assume : bool) : t * Target.t list =
      if is_empty tree
      then node_of_stem Path.empty stem failed_assume
      else
        let rec loop path parent finish =
          match path with
          | [] -> failwith "setting target with no path"
          | last_dir :: [] ->
            (* would step onto target node *)
            (* TODO: assert is target or assume *)
            let this_child = Children.child_exn parent.children last_dir in
            Format.printf "showing child where adding stem\n";
            Child.show this_child;
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
      | Pruned (* to signify end of tree in any way. We prune at max depth and when both children are collapsed *)
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
      let failed_assume_child = Child.make_failed_assume_child branch assumed_formulas in
      let failed_assume_target = Target.create @@ Path.append path branch.direction in
      let other_child = Child.make_target_child @@ Branch.Runtime.other_direction branch in
      let other_target = Target.create @@ Path.append path @@ Branch.Direction.other_direction branch.direction in
      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = failed_assume_child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = failed_assume_child }
      in
      children, failed_assume_target, other_target

    let of_branch (branch : Branch.Runtime.t) (node : Node.t) (path : Path.t) : t * Target.t =
      let child = Child.make_hit_node node in
      let other_child = Child.make_target_child @@ Branch.Runtime.other_direction branch in
      let children =
        match branch.direction with
        | True_direction  -> Both { true_side = child ; false_side = other_child }
        | False_direction -> Both { true_side = other_child ; false_side = child }
      in
      children, Target.create @@ Path.append path @@ Branch.Direction.other_direction branch.direction
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

    let show (x : t) : unit =
      Format.printf "%s\n" begin
        match x with
        | Hit _ -> "hit"
        | Target_acquired _ -> "target"
        | Waiting_to_pass_assume _ -> "assume"
        | Unsatisfiable -> "unsat"
        | Solver_timeout -> "timeout"

      end


    let node_exn (x : t) : Node.t =
      match x with
      | Hit node -> node
      | _ -> failwith "no node in node_exn"

    let make_hit_node (node : Node.t) : t =
      Hit node

    let make_failed_assume_child (branch : Branch.Runtime.t) (assumed_formulas : Formula_set.t) : t =
      Waiting_to_pass_assume
      { assumed_formulas = Formula_set.union assumed_formulas (Formula_set.singleton @@ Branch.Runtime.to_expr branch) }
      (* TODO: might not need to add the extra formula *)

    let make_target_child (branch : Branch.Runtime.t) : t =
      Target_acquired { constraints = Formula_set.singleton @@ Branch.Runtime.to_expr branch }

  end