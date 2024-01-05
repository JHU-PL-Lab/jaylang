open Jayil

module Status :
  sig
    type t =
      | Hit
      | Unhit
      | Unsatisfiable
      | Found_abort
      | Reach_max_step
      | Missed
      | Unreachable (* Only to be used when conveying final results. No branch is ever unreachable during evaluation *)

    val to_string : t -> string
  end

module Direction :
  sig
    type t =
      | True_direction
      | False_direction
      [@@deriving equal, compare, sexp]

    val to_string : t -> string
    val other_direction : t -> t
    (** [other_direction dir] gets the direction that is not [dir]. Trivial because
        there are only two variant constructors. *)
    val to_value_bool : t -> Ast.value
    (** [to_value_bool dir] gets the boolean associated with the direction
        as an AST value in the Jayil AST. *)
    val of_bool : bool -> t
end

module T :
  sig
    type t =
      { branch_ident : Ast.ident
      ; direction    : Direction.t }
  end

type t = T.t =
  { branch_ident : Ast.ident
  ; direction    : Direction.t }
  [@@deriving sexp, compare]
(** [t] is an AST branch with just the identifier of the clause. The runtime stack is not considered. *)

val to_string : t -> string
(** [to_string branch] ignores the condition of the branch and is the branch ident and direction. *)

val of_ident_and_bool : Ast.ident -> bool -> t
(** [of_ident_and_bool branch_ident dir] converts the bool [dir] to a direction and uses that to
    make a branch. *)

(*
  "Runtime" is a modifier/adjective on "Branch", so it is a "Runtime Branch".
*)
module Runtime :
  sig
    type t =
      { branch_key    : Lookup_key.t
      ; condition_key : Lookup_key.t
      ; direction     : Direction.t } [@@deriving compare, sexp]
    (** [t] is a branch in the AST during runtime, where its branch and condition both have a stack
        to identify them (hence they are a [Lookup_key.t]).
        The [branch_key] is the key of the clause to identify the node in the AST, and the [condition_key]
        is the key of the condition variable. 
        
        To use a runtime branch as a target for the concolic evaluator (because the concolic evaluator
        attempts to hit all directions of a branch), the [direction] might be switched to the other side. *)

    val to_expr : t -> Z3.Expr.expr
    (** [to_expr x] is a Z3 expression that sets the condition key in [x] to be equal to the direction as a bool. *)

    val to_string : t -> string
    (** [to_string x] shows all of the branch, condition, and direction in [x] as a string. *)

    val to_ast_branch : t -> T.t
    (** [to_ast_branch x] is an AST branch, where only the ident is kept from the branch key, and condition is discarded. *)

    val other_direction : t -> t
    (** [other_direction x] is a new branch keeping all attributes of [x], but [x.direction] is flipped. *)

    val print_target_option : t option -> unit
    (** [print_target_option x] prints the branch [x] as "target branch", or prints it as "None". *)

    val to_abort_pick_key : t -> Lookup_key.t
    (** [to_abort_pick_key branch] is a key that can be picked to set the [branch] as off limits if there
        is an abort under the branch. The formulas that handle such logic are not here. This is only the key
        that will be used in such a case. *)

    val to_max_step_pick_key : t -> Lookup_key.t
    (** [to_max_step_pick_key branch] is a key that can be picked to set the [branch] as off limits due to
        reaching max step un der the branch. See [to_abort_pick_key]. *)
  end

(*
  A `Status_store` tracks how AST branches are hit. It maps branch identifiers to their status.
*)
module Status_store :
  sig
    type t [@@deriving sexp, compare]
    (** [t] is a map from a branch identifier to the status of the branch. So it tells
        us whether the true and false direction of each branch have been hit. *)

    val empty : t
    (** [empty] has no information on any branches *)

    val print : t -> unit
    (** [print store] prints the statuses of all branches in the [store] to stdout. *)

    val add_branch_id : t -> Ast.ident -> t
    (** [add_branch_id store id] is a new store where the identifier [id] has been added to
        the branch store [store], and both directions of the new branch are unhit. See
        [Branch_status.both_unhit]. *)

    val get_unhit_branch : t -> T.t option
    (** [get_unhit_branch store] is some branch that is unhit. *)

    val set_branch_status : new_status:Status.t -> t -> T.t -> t
    (** [set_branch_status status store branch] is a new store where the given [branch] now has the
        [status]. All other branches are unaffected. *)

    val is_hit : t -> T.t -> bool
    (** [is_hit store branch] is true if and only if the status of [branch.branch_ident] in 
        the [store] has [branch.direction] as [Hit]. *)

    val get_status : t -> T.t -> Status.t
    (** [get_status store branch] is the status of the given [branch]. *)

    val find_branches : Ast.expr -> t -> t
    (** [find_branches e store] is a new store where all the branches in the given expression [expr]
        have been added as unhit branches to the given [store]. *)

    val finish : t -> t
    (** [finish store] is a new store where all unhit branches are now marked as unsatisfiable *)

  end