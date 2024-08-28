open Path_tree

module Status :
  sig
    type t =
      | Found_abort of (Branch.t * Jil_input.t list [@compare.ignore])
      | Type_mismatch of (Jil_input.t list [@compare.ignore])
      | Finished_interpretation of { pruned : bool }
      [@@deriving compare, sexp]
  end

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

    val get_status : t -> Status.t
    (** [get_status t] is the status of the (now finished) symbolic session. *)

    val is_reach_max_step : t -> bool
    (** [is_reach_max_step t] is true iff the interpretation of the dead [t] had hit the max step count. *)
  end

type t
(** [t] tracks symbolic representations of the program during interpretation. *)

val empty : t
(** [empty] is a default symbolic session. *)

val get_fun_depth : t -> Fun_depth.t
(** [get_fun_depth t] is the function depth of [t] used to make a concolic key. *)

val with_options : (t -> t) Options.Fun.t
(** [with_options t] is [t] configured with the optional arguments. *)

(*
  -----------
  EXPRESSIONS
  -----------
*)

val add_key_eq_val : t -> Concolic_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in [t]. *)

val add_alias : t -> Concolic_key.t -> Concolic_key.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in [t]. *)

val add_binop : t -> Concolic_key.t -> Jayil.Ast.binary_operator -> Concolic_key.t -> Concolic_key.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] in [t]. *)

val add_input : t -> Concolic_key.t -> Dvalue.t -> t
(** [add_input t x v] is [t] that knows input [x = v] was given. *)

val add_not : t -> Concolic_key.t -> Concolic_key.t -> t
(** [add_not t x y] adds [x = not y] to [t]. *)

val add_match : t -> Concolic_key.t -> Concolic_key.t -> Jayil.Ast.pattern -> t
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

val found_assume : t -> Concolic_key.t -> t
(** [found_assume t key] tells [t] that [key] is assumed to be true. *)

val fail_assume : t -> t
(** [fail_assume t] tells [t] that a recent assume/assert was false when it needs to be true. *)

val found_abort : t -> t
(** [found_abort t] tells [t] that an abort was found in interpretation. *)

val found_type_mismatch : t -> t
(** [found_type_mismatch t] tells [t] that there was a type mismatch in interpretation. *)

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
