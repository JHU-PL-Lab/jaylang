(* open Core *)

(*
  Currently I have a type error here as I work this out.

  What I am doing is basically checking the whole tree each time
  if I could collapse. What I need to do is merge two nodes and
  get a message that says whether both were collapsed. If they were
  both collapsed, then we can collapse this.

  But really what we have is:
  * Merging statuses: if either has been collapsed, then we can forget about it and just collapse
      * This is because we only collapse if we know we have exhausted everything below. There is no point in keeping that unnecessary status if we have exhausted it before
  * Merging child: this is the same as merging a status because a child is a status with more info
  * Merging children: 
    * if one set of children have already been collapsed, then keep that
    * if one set is none, then keep the other
    * recursively call merge on the left and right of each.
      * if both end up as collapsed, then collapse. Otherwise, leave as is
  * Merging nodes:
    * recursively merge the children
    * if collapsed, then return that up (so that status can call collapse)

  What I should do is limit the merging to merge a tree with a single path. So I might need a
  single path type (which is really just a linked list) down the tree, and we merge trees with
  paths.

  Also I would want an update feature, where I can update a node at the end of the path. When I update,
  there is possibility of collapse.

  When I merge, I think I might still collapse in case that new hit node is a leaf in the tree, and
  that is what allows us to collapse.

  This motivates a change to merging:
    * I can only merge by giving a new path to add starting from another path. i.e. add a stem
    * So we take a tree, a path to where to attach the stem, and a stem

  So I think I should start with a new data structure, where it's easier to track what has been collapsed
  and easier to add stems
  * I want a node with formulas and children
  * The children should maybe have more status to them
    * We only ever have no children when interpretation finished, we hit a failed assume/assert, or the tree was pruned
    * So we can have children be "end of tree", "end of interpretation", "waiting to pass assume", or have the actual children
    * End of tree and end of interpretation are the exact same, I guess. They can be deleted
  * In place of unsolved, I'd like to have "active target"
  * The children really don't need to be identified with a key except just to be sure of correctness
    * Instead, it is equivalent to just have true/false sides
  * And I'd like to acquire the targets here. Just give a path of branches taken, and this returns the new targets.
    * Makes it much easier externally to track all this
  * Instead of building a node stack in the symbolic session, I'd prefer to keep a stack of formulas attained.
    * This is a list of formula sets and contains no constraints
    * The stack only begins after we've reached the depth of the target
    * Then just say to add this as a stem from the target, and tell the path tree whether it ended in failed assume or not
    * The path tree returns a new tree and the list of new targets
    * Note that the stack of formulas does need to have branches on it in order to add the contraints properly once handed over to the path tree
    * So the stack is (Branch.Runtime.t * Formula_set.t) list
      * Think about whether I really need it to be nonempty
        * I do because I need global formulas before first branch
*)

(*
  -----------------
  MODULE SIGNATURES   
  -----------------

  We have lowercase type names for all types in recursive modules. We use destructive
  substitution to fill the types in when they're actually used as module signatures in the
  recursive modules.
*)

(* module type NODE =
  sig
    type children
    type child
    type status
    type t =
      { formulas : Formula_set.t
      ; children : children } [@@deriving compare]
    (** [t] is the root of the JIL program. *)
    val empty : t
    (** [empty] is the tree before the program has ever been run. It has no formulas or children. *)
    val merge_and_collapse : t -> t -> t * bool
    val merge : t -> t -> t
    (** [merge a b] combines the trees [a] and [b] and throws an exception if there is a discrepancy. *)
    val add_formula : t -> Z3.Expr.expr -> t
    (** [add_formulas t expr] is [t] that has gained [expr] as a formula. *)
    val get_child : t -> Branch.Runtime.t -> child option
    (** [get_child t branch] is the child of [t] by taking the [branch], if it exists. *)
    val get_child_exn : t -> Branch.Runtime.t -> child
    (** [get_child_exn t branch] is the child of [t] by taking the [branch], or exception. *)
    val is_valid_target_child : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target child from [t]. *)
    val with_formulas : t -> Formula_set.t -> t
    (** [with_formulas t formulas] is [t] with the given [formulas] overwriting its old formulas. *)
    val set_status : t -> Branch.Runtime.t -> status -> Path.t -> t
    (** [set_status t branch status path] is [t] where child at [branch] is given [status], and [branch] is necessarily
        found along the [path]. *)
  end
  

module type CHILDREN = 
  sig
    type node
    type child
    type t [@@deriving compare]
    (** [t] represents the branches underneath some node. *)
    val empty : t
    (** [empty] is no children. *)
    val is_empty : t -> bool
    val set_node : t -> Branch.Runtime.t -> node -> t
    (** [add t branch child] adds [child] as a node underneath the [branch] in [t]. *)
    val merge_and_collapse : t -> t -> t * bool
    val get_child : t -> Branch.Runtime.t -> child option
    (** [get_child t branch] is the child in [t] by taking the [branch]. *)
    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target. *)
    val set_child : t -> child -> t
    (* val is_collapsible : t -> bool *)
  end

module type CHILD =
  sig
    type status
    type node
    type t =
      { status      : status
      ; constraints : Formula_set.t
      ; branch      : Branch.Runtime.t
      } [@@deriving compare]
    (** [t] is a single child of a [Node.t] *)
    val create : node -> Branch.Runtime.t -> t
    (** [create node branch] makes a child by taken the [branch] to reach the given [node]. *)
    val create_both : node -> Branch.Runtime.t -> t * t
    (** [create_both node branch] makes a child by taking the [branch] to reach the given [node]. 
        Also returns the other side as unsolved. **Note** [node] is the child, not the parent. *)
    val merge_and_collapse : t -> t -> t * bool
    val is_valid_target : t -> bool
    val to_node_exn : t -> node
    val unsolved : Branch.Runtime.t -> t
    val to_formulas : t -> Z3.Expr.expr list
    val map_node : t -> f:(node -> node) -> t
    val is_hit : t -> bool
  end
  
module type STATUS =
  sig
    type node
    type t =
      | Collapsed
      | Hit of node
      | Unsatisfiable
      | Failed_assume
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
      [@@deriving compare]
    (** [t] is a node during a solve. It has been hit, determined unsatisfiable,
        is not known if hittable or unsatisfiable, or has not been solved or seen yet.
        Unsatisfiable or Unknown nodes are status of the node before they've ever been
        hit during interpretation, so there is no existing node as a payload. *)

    val merge_and_collapse : t -> t -> t * bool

    val is_valid_target : t -> bool
  end


(*
  --------------------------------
  RECURSIVE MODULE IMPLEMENTATIONS   
  --------------------------------
*)

module rec Node : (* serves as root node *)
  NODE with
  type children := Children.t and
  type child    := Child.t and
  type status   := Status.t 
  =
  struct
    type t =
      { formulas : Formula_set.t
      ; children : Children.t } [@@deriving compare]

    let empty : t =
      { formulas = Formula_set.empty
      ; children = Children.empty }
    
    (* let is_collapsible (x : t) : bool =
      Children.is_collapsible x.children *)

    let merge_and_collapse (a : t) (b : t) : t * bool =
      let children, collapsed = Children.merge_and_collapse a.children b.children in
      { formulas = Formula_set.union a.formulas b.formulas
      ; children }, collapsed

    let merge (a : t) (b : t) : t =
      Tuple2.get1
      @@ merge_and_collapse a b

    let add_formula (x : t) (expr : Z3.Expr.expr) : t =
      { x with formulas = Formula_set.add x.formulas expr }

    let get_child (x : t) (branch : Branch.Runtime.t) : Child.t option =
      Children.get_child x.children branch

    let get_child_exn (x : t) (branch : Branch.Runtime.t) : Child.t =
      Option.value_exn
      @@ get_child x branch

    let is_valid_target_child (x : t) (branch : Branch.Runtime.t) : bool =
      Children.is_valid_target x.children branch

    let with_formulas (x : t) (formulas : Formula_set.t) : t =
      { x with formulas }

    let set_status (x : t) (branch : Branch.Runtime.t) (status : Status.t) (path : Path.t) : t =
      let rec loop node path d =
        match path with
        | [] -> begin (* at end of path, so target should be a child of this node *)
          match get_child node branch with
          | Some target_child ->
            { node with children = Children.set_child node.children { target_child with status } }
          | None -> failwith "bad path in set status"
          end
        | hd :: tl -> (* continue down path *)
          let old_child = get_child_exn node hd in (* is Hit next_node *)
          let result_node = loop (Child.to_node_exn old_child) tl (d + 1) in 
          let new_child = { old_child with status = Hit result_node } in (* must do this to keep constraints of old child *)
          { node with children = Children.set_child node.children new_child }
      in
      loop x path.forward_path 1
  end (* Node *)
and Children :
  CHILDREN with
  type node  := Node.t and
  type child := Child.t
  =
  struct
    type t = 
      | Collapsed
      | No_children
      | Both of { true_side : Child.t ; false_side : Child.t ; branch_key : Concolic_key.t } [@@deriving compare]
      (* Could have chosen to have only true or only false, but Status.Unsolved takes care of that. *)

    let empty : t = No_children

    let is_empty (x : t) : bool =
      match x with
      | Collapsed
      | No_children -> true
      | _ -> false

    let set_child (x : t) (child : Child.t) : t =
      let other = Child.unsolved @@ Branch.Runtime.other_direction child.branch in
      match child.branch.direction with
      | True_direction -> begin
        match x with
        | Collapsed -> failwith "setting child in collapsed"
        | No_children -> Both { true_side = child ; false_side = other ; branch_key = child.branch.branch_key }
        | Both r -> Both { r with true_side = child }
      end
      | False_direction -> begin
        match x with
        | Collapsed -> failwith "setting child in collapsed"
        | No_children -> Both { true_side = other ; false_side = child ; branch_key = child.branch.branch_key }
        | Both r -> Both { r with false_side = child }
      end

    let set_node (x : t) (branch : Branch.Runtime.t) (node : Node.t) : t =
      match x with
      | Collapsed -> failwith "setting child in collapsed"
      | No_children ->
        let left, right = Child.create_both node branch in
        Both { true_side = left ; false_side = right ; branch_key = branch.branch_key }
      | Both r -> begin
        match branch.direction with
        | True_direction -> Both { r with true_side = Child.create node branch }
        | False_direction -> Both { r with false_side = Child.create node branch }
      end

    let merge_and_collapse (a : t) (b : t) : t * bool =
      match a, b with
      | Collapsed, _
      | _, Collapsed -> Collapsed, true
      | x, No_children
      | No_children, x -> x, false
      | Both a, Both b ->
        if Concolic_key.compare a.branch_key b.branch_key <> 0
        then failwith "unequal branches in merging children";
        let true_side, true_collapsed = Child.merge_and_collapse a.true_side b.true_side in
        let false_side, false_collapsed = Child.merge_and_collapse a.false_side b.false_side in
        if true_collapsed && false_collapsed
        then Collapsed, true
        else
          Both { true_side ; false_side ; branch_key = a.branch_key }, false

    let get_child (x : t) (branch : Branch.Runtime.t) : Child.t option =
      match x, branch.direction with
      | Both { true_side = child ; branch_key ; _ }, Branch.Direction.True_direction
      | Both { false_side = child ; branch_key ; _ }, Branch.Direction.False_direction ->
          if Concolic_key.compare branch_key branch.branch_key <> 0
          then None
          else Some child
      | _ -> None

    let is_valid_target (x : t) (branch : Branch.Runtime.t) : bool =
      match x, branch.direction with
      | Both { true_side = child ; _ }, Branch.Direction.True_direction
      | Both { false_side = child ; _ }, Branch.Direction.False_direction -> Child.is_valid_target child
      | Collapsed, _ -> false
      | No_children, _ -> failwith "child does not exist"
  end (* Children *)
and Child : 
  CHILD with
  type status := Status.t and
  type node   := Node.t
  =
  struct
    type t =
      { status      : Status.t
      ; constraints : Formula_set.t [@compare.ignore] (* for assumes, asserts, etc that must be satisfied when entering this child *)
      ; branch      : Branch.Runtime.t (* branch taken to reach the child *)
      } [@@deriving compare]

    let create (node : Node.t) (branch : Branch.Runtime.t) : t =
      { status = Status.Hit node
      ; constraints = Formula_set.singleton @@ Branch.Runtime.to_expr branch
      ; branch }

    (* makes true side, false side, where the given branch is hit, and the other is unsolved *)
    let create_both (node : Node.t) (branch : Branch.Runtime.t) : t * t =
      let this_side = create node branch in
      let other_branch = Branch.Runtime.other_direction branch in
      let other_side =
        { status = Status.Unsolved
        ; constraints = Formula_set.singleton @@ Branch.Runtime.to_expr other_branch
        ; branch = other_branch }
      in
      match branch.direction with
      | True_direction -> this_side, other_side
      | False_direction -> other_side, this_side

    let merge_and_collapse (a : t) (b : t) : t * bool =
      let new_status, collapsed = Status.merge_and_collapse a.status b.status in
      { status = new_status
      ; constraints = Formula_set.union a.constraints b.constraints
      ; branch = a.branch }, collapsed

    let is_valid_target ({ status ; _ } : t) : bool = 
      Status.is_valid_target status

    let to_node_exn ({ status ; _ } : t) : Node.t =
      match status with
      | Hit node -> node
      | _ -> failwith "no node in Child.to_node_exn"

    let unsolved (branch : Branch.Runtime.t) : t =
      { status = Unsolved
      ; constraints = Formula_set.singleton @@ Branch.Runtime.to_expr branch
      ; branch }

    let to_formulas (x : t) : Z3.Expr.expr list =
      Formula_set.to_list x.constraints
      @ match x.status with
        | Hit node -> Formula_set.to_list node.formulas
        | _ -> []

    let map_node (x : t) ~(f : Node.t -> Node.t) : t =
      match x.status with
      | Hit node -> { x with status = Hit (f node) }
      | _ -> x

    let is_hit ({ status ; _ } : t) : bool =
      match status with
      | Hit _
      | Failed_assume -> true
      | _ -> false

  end (* Child *)
and Status :
  STATUS with
  type node := Node.t
  =
  struct
    type t =
      | Collapsed
      | Hit of Node.t
      | Unsatisfiable
      | Failed_assume (* node was hit but has since failed an assume/assert *)
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
      [@@deriving compare]

    let collapse (x : t) : t =
      match x with
      | Collapsed
      | Unsatisfiable -> Collapsed
      | Hit node when Children.is_empty node.children -> Collapsed
      | _ -> x

    let merge_and_collapse (a : t) (b : t) : t * bool =
      match collapse a, collapse b with
      | Collapsed, _
      | _, Collapsed -> Collapsed, true
      | Hit n1, Hit n2 ->
        (* We now optimize this away, but this assert always did pass before the optimization *)
        (* if not (Formula_set.equal n1.formulas n2.formulas)
        then failwith "formula sets not equal in merge of Status"; *) (* formula sets should only ever be equivalent after any visit to the same node *)
        let merged, is_collapsed = Node.merge_and_collapse n1 n2 in
        if is_collapsed
        then Collapsed, true
        else Hit merged, false
      | Hit node, _ | _, Hit node -> Hit node, false
      | Unsatisfiable, _ | _, Unsatisfiable -> Unsatisfiable, false
      | Failed_assume, _ | _, Failed_assume -> Failed_assume , false
      | Unknown, _ | _, Unknown -> Unknown, false
      | Unsolved, _ -> Unsolved, false

    (*
      Merge by keeping the most info.
      * It is most information to know that we have hit a node. Merge the nodes if necessary.
      * Next is to have solved and determined unsatisfiable
      * It is less knowledge to know that we have hit a node but failed assume, so we couldn't get further.
        It's intended that these nodes will be solved for again and determined as (fully) Hit or Unsatisfiable.
      * After that is solved by timed out, so unknown
      * After that is completely unsolved, which is no information at all
    *)

    let is_valid_target (x : t) : bool =
      match x with
      | Unsolved
      | Failed_assume -> true
      | _ -> false
  end (* Status *)

(* This is just for better naming *)
module Root = Node *)