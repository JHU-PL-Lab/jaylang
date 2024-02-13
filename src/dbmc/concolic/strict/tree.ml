open Core

(*
  -----------------
  MODULE SIGNATURES   
  -----------------

  We have lowercase type names for all types in recursive modules. We use destructive
  substitution to fill the types in when they're actually used as module signatures in the
  recursive modules.
*)

module type NODE =
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
    (* val add_child : t -> Branch.Runtime.t -> t *)
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
    val set_status : t -> child -> status -> Path.t -> t
    (** [set_status t child status path] is [t] where [child] is given [status], and [child] is necessarily
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
    val merge : t -> t -> t
    (** [merge a b] merges all children in [a] and [b]. *)
    val get_child : t -> Branch.Runtime.t -> child option
    (** [get_child t branch] is the child in [t] by taking the [branch]. *)
    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target. *)
    val set_child : t -> child -> t
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
    val merge : t -> t -> t
    val is_valid_target : t -> bool
    val to_node_exn : t -> node
    val unsolved : Branch.Runtime.t -> t
    val to_formulas : t -> Z3.Expr.expr list
    val map_node : t -> f:(node -> node) -> t
  end
  
module type STATUS =
  sig
    type node
    type t =
      | Hit of node
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


(*
  --------------------------------
  RECURSIVE MODULE IMPLEMENTATIONS   
  --------------------------------
*)

module rec Node : (* serves as root node *)
  NODE with
  type children := Children.t and
  type child := Child.t and
  type status := Status.t 
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
      { formulas = Formula_set.union a.formulas b.formulas
      ; children = Children.merge a.children b.children }

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

    let set_status (x : t) (child : Child.t) (status : Status.t) (path : Path.t) : t =
      let ret_child = { child with status } in
      let rec loop node path =
        match get_child node ret_child.branch with
        | Some target_child -> { node with children = Children.set_child node.children ret_child }
        | None -> begin (* didn't immediately find desired child, so continue down path *)
          match path with
          | branch :: tl -> 
            let old_child = get_child_exn node branch in (* is Hit next_node *)
            let result_node = loop (Child.to_node_exn old_child) tl in 
            let new_child = { old_child with status = Hit result_node } in (* must do this to keep constraints of old child *)
            { node with children = Children.set_child node.children new_child }
          | [] -> failwith "bad path in set status"
        end
      in
      loop x path
  end (* Node *)
and Children :
  CHILDREN with
  type node := Node.t and
  type child := Child.t
  =
  struct
    type t = 
      | No_children
      | Both of { true_side : Child.t ; false_side : Child.t ; branch_key : Lookup_key.t } [@@deriving compare]
      (* Could have chosen to have only true or only false, but Status.Unsolved takes care of that. *)
    
    let empty : t = No_children
    let is_empty (x : t) : bool =
      match x with
      | No_children -> true
      | _ -> false

    let set_child (x : t) (child : Child.t) : t =
      let other = Child.unsolved @@ Branch.Runtime.other_direction child.branch in
      match child.branch.direction with
      | True_direction -> begin
        match x with
        | No_children -> Both { true_side = child ; false_side = other ; branch_key = child.branch.branch_key }
        | Both r -> Both { r with true_side = child }
      end
      | False_direction -> begin
        match x with
        | No_children -> Both { true_side = other ; false_side = child ; branch_key = child.branch.branch_key }
        | Both r -> Both { r with false_side = child }
      end

    let set_node (x : t) (branch : Branch.Runtime.t) (node : Node.t) : t =
      match x with
      | No_children ->
        let left, right = Child.create_both node branch in
        Both { true_side = left ; false_side = right ; branch_key = branch.branch_key }
      | Both r -> begin
        match branch.direction with
        | True_direction -> Both { r with true_side = Child.create node branch }
        | False_direction -> Both { r with false_side = Child.create node branch }
      end


    let merge (a : t) (b : t) : t =
      match a, b with
      | No_children, x
      | x, No_children -> x
      | Both a, Both b ->
        if Lookup_key.compare a.branch_key b.branch_key <> 0
        then failwith "unequal branches in merging children";
        Both
        { true_side = Child.merge a.true_side b.true_side
        ; false_side = Child.merge a.false_side b.false_side
        ; branch_key = a.branch_key }

    let get_child (x : t) (branch : Branch.Runtime.t) : Child.t option =
      match x, branch.direction with
      | Both { true_side = child ; branch_key ; _ }, Branch.Direction.True_direction
      | Both { false_side = child ; branch_key ; _ }, Branch.Direction.False_direction ->
          if Lookup_key.compare branch_key branch.branch_key <> 0
          then None
          else Some child
      | _ -> None

    let is_valid_target (x : t) (branch : Branch.Runtime.t) : bool =
      match x, branch.direction with
      | Both { true_side = child ; _ }, Branch.Direction.True_direction
      | Both { false_side = child ; _ }, Branch.Direction.False_direction -> Child.is_valid_target child
      | No_children, _ -> true (* no children *) 
  end (* Children *)
and Child : 
  CHILD with
  type status := Status.t and
  type node := Node.t
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

    let merge (a : t) (b : t) : t =
      if Branch.Runtime.compare a.branch b.branch <> 0
      then failwith "merging unequal branches";
      { status = Status.merge a.status b.status
      ; constraints = Formula_set.union a.constraints b.constraints
      ; branch = a.branch }

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
  end (* Child *)
and Status :
  STATUS with
  type node := Node.t
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
      | Hit n1, Hit n2 ->
        if not (Formula_set.equal n1.formulas n2.formulas)
        then failwith "formula sets not equal in merge of Status"; (* formula sets should only ever be equivalent after any visit to the same node *)
        Hit (Node.merge n1 n2)
      | Hit node, _ | _, Hit node -> Hit node
      | Unsatisfiable, _ | _, Unsatisfiable -> Unsatisfiable
      | Unknown, _ | _, Unknown -> Unknown
      | Unsolved, _ -> Unsolved

    let is_valid_target (x : t) : bool =
      match x with
      | Unsolved -> true
      | _ -> false
  end (* Status *)

(* This is just for better naming *)
module Root = Node