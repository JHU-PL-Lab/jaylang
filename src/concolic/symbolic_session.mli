open Path_tree

module Dead :
  sig
    type t
    (** [t] is a symbolic session that can no longer be used during interpretation. *)

    (*
      ---------
      ACCESSORS   
      ---------
    *)

    val root : t -> Root.t
    (** [root t] is the root from the dead [t]. *)

    val targets : t -> Target.t list
    (** [targets t] is the targets in the dead [t]. *)

    val branch_info : t -> Branch_info.t
    (** [branch_info t] is current branch info from [t]. *)

    val hit_max_depth : t -> bool
    (** [hit_max_depth t] is true iff [t] reached the max allowed tree depth. *)

    val is_reach_max_step : t -> bool
    (** [is_reach_max_step t] is true iff [t] reached the allowed max step during interpretation. *)
  end

type t
(** [t] tracks symbolic representations of the program during interpretation. *)

val empty : t
(** [empty] is a default symbolic session. *)

val get_key_depth : t -> int
(** [get_key_depth t] is the depth of [t] used to make a concolic key. *)

val with_options : (t -> t) Options.Fun.t
(** [with_options t] is [t] configured with the optional arguments. *)

(*
  -----------
  EXPRESSIONS
  -----------
*)

val add_key_eq_val : t -> Concolic_key.Lazy.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in [t]. *)

val add_alias : t -> Concolic_key.Lazy.t -> Concolic_key.Lazy.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in [t]. *)

val add_binop : t -> Concolic_key.Lazy.t -> Jayil.Ast.binary_operator -> Concolic_key.Lazy.t -> Concolic_key.Lazy.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] in [t]. *)

val add_input : t -> Concolic_key.Lazy.t -> Dvalue.t -> t
(** [add_input t x v] is [t] that knows input [x = v] was given. *)

val add_not : t -> Concolic_key.Lazy.t -> Concolic_key.Lazy.t -> t
(** [add_not t x y] adds [x = not y] to [t]. *)

val add_match : t -> Concolic_key.Lazy.t -> Concolic_key.Lazy.t -> Jayil.Ast.pattern -> t
(** [add_match t x y pat] adds [x = y ~ pat] to [t]. *)

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : t -> Branch.Runtime.t -> t
(** [hit_branch t branch] is [t] that knows [branch] has been hit during interpretation. *)

val enter_fun : t -> t
(** [enter_fun t] is [t] that knows function depth has increased by 1. *)

val found_assume : t -> Concolic_key.Lazy.t -> t
(** [found_assume t key] tells [t] that [key] is assumed to be true. *)

val fail_assume : t -> t
(** [fail_assume t] tells [t] that a recent assume/assert was false when it needs to be true. *)

val found_abort : t -> t
(** [found_abort t] tells [t] that an abort was found in interpretation. *)

val found_type_mismatch : t -> Jayil.Ast.Ident_new.t -> t
(** [found_type_mismatch t id] tells [t] that there was a type mismatch at clause [id]. *)

val reach_max_step : t -> t
(** [reach_max_step t] tells [t] that the max interpretation step was hit, and interpretation stopped. *)

(*
  -----------
  BETWEEN-RUN   
  -----------
*)

val finish : t -> Root.t -> Dead.t
(** [finish t root] creates a finished session from [t] that merges info with the given [root].
    The merged result can be gotten with [root_exn @@ finish t root]. *)

val make : Root.t -> Target.t -> t
(** [make root target] makes an empty t that knows the given [root] and [target]. *)
