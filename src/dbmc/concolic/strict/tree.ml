open Core

module rec Node : (* serves as root node *)
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
    val get_child : t -> Branch.Runtime.t -> Child.t option
    (** [get_child t branch] is the child of [t] by taking the [branch], if it exists. *)
    val get_child_exn : t -> Branch.Runtime.t -> Child.t
    (** [get_child_exn t branch] is the child of [t] by taking the [branch], or exception. *)
    val is_valid_target_child : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target child from [t]. *)
    val with_formulas : t -> Formula_set.t -> t
    (** [with_formulas t formulas] is [t] with the given [formulas] overwriting its old formulas. *)
    val set_status : t -> Child.t -> Status.t -> Path.t -> t
    (** [set_status t child status path] is [t] where [child] is given [status], and [child] is necessarily
        found along the [path]. *)

    val contains_unsat : t -> bool
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

    let get_child (x : t) (branch : Branch.Runtime.t) : Child.t option =
      Children.get_child x.children branch

    let get_child_exn (x : t) (branch : Branch.Runtime.t) : Child.t =
      Option.value_exn
      @@ get_child x branch

    let is_valid_target_child (x : t) (branch : Branch.Runtime.t) : bool =
      Children.is_valid_target x.children branch

    let with_formulas (x : t) (formulas : Formula_set.t) : t =
      { x with formulas }

    let contains_unsat ({ children ; _ } : t) : bool =
      Children.contains_unsat children

    let set_status (x : t) (child : Child.t) (status : Status.t) (path : Path.t) : t =
      let ret_child = { child with status } in
      let rec loop node path =
        match get_child node ret_child.branch with
        | Some target_child -> { node with children = Children.set_child node.children ret_child }
        | None -> begin (* didn't immediately find desired child, so continue down path *)
          match path with
          | branch :: tl -> 
            let step = Child.to_node_exn @@ get_child_exn node branch in
            { node with children = Children.set_node node.children branch @@ loop step tl } (* TODO: fix how we lose constraints here *)
          | [] -> failwith "bad path in set status"
        end
      in
      loop x path
      (* let new_root = loop x path in *)
      (* Format.printf "Returning from set_status. Contains unsat = %b\n" (contains_unsat new_root); *)
      (* new_root *)
      (* let new_root = loop x path in
      let rec travel_path node = function
        | branch :: tl ->
          let step = get_child_exn node branch in
          begin
          match get_child node ret_child.branch with
          | Some target_child ->
            begin
            match target_child.status with
            | Unknown | Unsatisfiable -> failwith "found unsatisfiable or unknown"
            | _ -> ()
            end;
          | None -> ()
          end;
          begin
          match step.status with
          | Unknown | Unsatisfiable -> failwith "found unsatisfiable or unknown on path"
          | _ -> ()
          end;
          travel_path (Child.to_node_exn step) tl
        | [] -> failwith "end of path"
      in
      travel_path new_root path *)


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
    val set_node : t -> Branch.Runtime.t -> Node.t -> t
    (** [add t branch child] adds [child] as a node underneath the [branch] in [t]. *)
    val merge : t -> t -> t
    (** [merge a b] merges all children in [a] and [b]. *)
    val get_child : t -> Branch.Runtime.t -> Child.t option
    (** [get_child t branch] is the child in [t] by taking the [branch]. *)
    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target. *)
    val set_child : t -> Child.t -> t

    val contains_unsat : t -> bool
  end
  =
  struct
    type t = 
      | No_children
      | Both of { true_side : Child.t ; false_side : Child.t ; branch_key : Lookup_key.t } [@@deriving compare]
      (* Could have chosen to have only true or only false, but Status.Unsolved takes care of that. *)
    
    let empty : t = No_children
    (* let is_empty (x : t) : bool =
      match x with
      | No_children -> true
      | _ -> false *)

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

    (* let of_node ({ this_branch ; base } as node : Node.t) : t =
      let new_node = Status.Hit node in
      match this_branch with
      | { direction = True_direction ; _ } -> Both { true_side = new_node ; false_side = Unsolved }
      | { direction = False_direction ; _ } -> Both { true_side = Unsolved ; false_side = new_node } *)

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

    let contains_unsat = function
      | No_children -> false
      | Both { true_side ; false_side ; _ } -> let a = Child.contains_unsat true_side in let b = Child.contains_unsat false_side in a || b
  end
and Child : 
  sig
    type t =
      { status      : Status.t
      ; constraints : Formula_set.t
      ; branch      : Branch.Runtime.t
      } [@@deriving compare]
    (** [t] is a single child of a [Node.t] *)
    val create : Node.t -> Branch.Runtime.t -> t
    (** [create node branch] makes a child by taken the [branch] to reach the given [node]. *)
    val create_both : Node.t -> Branch.Runtime.t -> t * t
    (** [create_both node branch] makes a child by taking the [branch] to reach the given [node]. 
        Also returns the other side as unsolved. **Note** [node] is the child, not the parent. *)
    val merge : t -> t -> t
    val is_valid_target : t -> bool
    val to_node_exn : t -> Node.t
    val unsolved : Branch.Runtime.t -> t
    val to_formulas : t -> Z3.Expr.expr list

    val contains_unsat : t -> bool
  end
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

    let contains_unsat = function
      | { status = Unsatisfiable ; branch ; _ } -> Format.printf "Found unsat at %s\n" (Branch.Runtime.to_string branch); true
      | { status = Hit node ; _ } -> Node.contains_unsat node
      | _ -> false
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
(* and Node :
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

  end *)

(* This is just for better naming *)
module Root = Node