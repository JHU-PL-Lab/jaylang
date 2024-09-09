
module rec Node :
  sig
    type t =
      { formulas : Formula_set.t (* formulas for all clauses in the node *)
      ; children : Children.t } [@@deriving compare]
    (** [t] is the root of the JIL program and the body of any hit child nodes in the tree. *)
    val empty : t
    (** [empty] is the tree before the program has ever been run. It has no formulas or children. *)

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

    val set_status : t -> Branch.Runtime.t -> Status.t -> Path.t -> t
    (** [set_status t branch status path] is [t] where child at [branch] is given [status], and [branch] is necessarily
        found along the [path]. *)
  end
and Children :
  sig
    type t [@@deriving compare]
    (** [t] represents the branches underneath some node. *)

    val empty : t
    (** [empty] is no children. *)

    val is_empty : t -> bool
    (** [is_empty t] is true if and only if [t] is [empty]. *)

    val set_node : t -> Branch.Runtime.t -> Node.t -> t
    (** [set_node t branch child] adds [child] as a node underneath the [branch] in [t]. *)

    val get_child : t -> Branch.Runtime.t -> Child.t option
    (** [get_child t branch] is the child in [t] by taking the [branch]. *)

    val is_valid_target : t -> Branch.Runtime.t -> bool
    (** [is_valid_target t branch] is [true] if and only if [branch] should be a target. *)

    val set_child : t -> Child.t -> t
    (** [set_child t child] sets [t] to gain [child], overwriting any child with the same branch as [child.branch]. *)
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

    val is_valid_target : t -> bool
    (** [is_valid_target t] is [Status.is_valid_target t.status] *)

    val to_node_exn : t -> Node.t
    (** [to_node_exn t] is [node] where [t.status] matches [Hit node], or exception. *)

    val unsolved : Branch.Runtime.t -> t
    (** [unsolved branch] is unsolved node with no constraints and has the given [branch]. *)

    val to_formulas : t -> Z3.Expr.expr list
    (** [to_formulas t] is all constraints in [t] and formulas inside [node] if [t.status] is [Hit node]. *)

    val map_node : t -> f:(Node.t -> Node.t) -> t
    (** [map_node t ~f] is [t] if [t.status] is not [Hit node], otherwise maps [node] via [f]. *)

    val is_hit : t -> bool
    (** [is_hit t] is true if and only if some interpretation has hit [t], i.e. it's Hit or Failed_assume *)
  end
and Status :
  sig
    type t =
      | Collapsed
      | Hit of Node.t
      | Unsatisfiable
      | Failed_assume (* Node was hit but failed assume shortly after hitting. Node can be determined unsatisfiable after this due to not being able to satisfy the assume *)
      | Unknown (* for timeouts *)
      | Unsolved (* not yet tried *)
      [@@deriving compare]
    (** [t] is a node during a solve. It has been hit, determined unsatisfiable,
        is not known if hittable or unsatisfiable, or has not been solved or seen yet.
        Unsatisfiable or Unknown nodes are status of the node before they've ever been
        hit during interpretation, so there is no existing node as a payload. *)

    val is_valid_target : t -> bool
    (** [is_valid_target t] is true if and only if [t] should be targeted in a concolic run. *)
  end

module Root = Node (* for better naming *)