open Jayil

module Status :
  sig
    type t =
      | Hit
      | Unhit
      | Unsatisfiable
      | Reached_max_step
      | Unreachable
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

module Branch_status :
  sig
    type t
    (** [t] is a branch direction and its status. *)
    
    val both_unhit : t
    (** [both_unhit] is a branch status that has both directions as unhit. *)

    val hit : t -> Direction.t -> t
    (** [hit x dir] is a new branch status with the direction [dir] now hit. The
        other direction gets the same status as [x]. *)

    val to_string : t -> Ast.ident -> string
    val print : t -> Ast.ident -> unit
  end

module T :
  sig
    type t = 
      { branch_ident : Ast.ident
      ; direction : Direction.t }
    (** [t] has some variable identifier for the branch (the variable in the branch clause, not
        the condition variable), and some specific direction. i.e. a branch is not just the clause,
        but it is specifically some direction of the clause. *)

    val to_string : t -> string
  end

type t = T.t =
  { branch_ident : Ast.ident
  ; direction : Direction.t }

val to_string : t -> string
val of_ident_and_bool : Ast.ident -> bool -> t


module Status_store :
  sig
    type t = Branch_status.t Ast.Ident_map.t
    (** [t] is a map from a branch identifier to the status of the branch. So it tells
        us whether the true and false direction of each branch have been hit. *)

    val empty : t
    (** [empty] has no information on any branches *)

    val print : t -> unit

    val add_branch_id : t -> Ast.ident -> t
    (** [add_branch_id store id] is a new store where the identifier [id] has been added to
        the branch store [store], and both directions of the new branch are unhit. See
        [Branch_status.both_unhit]. *)

    val get_unhit_branch : t -> T.t option
    (** [get_unhit_branch store] is some branch that is unhit. *)

    val hit_branch : t -> T.t -> t
    (** [hit_branch store branch] is a new store where the given branch is now hit. All other
        branches are unaffected. *)

    val set_unsatisfiable : t -> T.t -> t
    (** [set_unsatisfiable store branch] is a new store where the given branch is marked unsatisfiable. *)

    val set_reached_max_step : t -> T.t -> t
    (** [set_reached_max_step store branch] is a new store where the given branch is marked such that
        the branch was attempted to be hit, but the max step was exceeded. *)

    val is_hit : t -> T.t -> bool
    (** [is_hit store branch] is true if and only if the status of [branch.branch_ident] in 
        the [store] has [branch.direction] as [Hit]. *)

    val find_branches : Ast.expr -> t -> t
    (** [find_branches e store] is a new store where all the branches in the given expression [expr]
        have been added as unhit branches to the given [store]. *)
  end
