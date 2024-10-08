
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

    val root : t -> Path_tree.t
    (** [root t] is the root from the dead [t]. *)

    val get_status : t -> Status.t
    (** [get_status t] is the status of the (now finished) symbolic session. *)

    val is_reach_max_step : t -> bool
    (** [is_reach_max_step t] is true iff the interpretation of the dead [t] had hit the max step count. *)
  end

type t
(** [t] tracks symbolic representations of the program during interpretation. *)

val empty : t
(** [empty] is a default symbolic session. *)

val with_options : (t, t) Options.Fun.a
(** [with_options t] is [t] configured with the optional arguments. *)

val get_max_step : t -> int
(** [get_max_step t] is the number of steps [t] expects interpretations to max out at. *)

val get_feeder : t -> Concolic_feeder.t
(** [get_feeder t] is the feeder for an interpretation alongside [t]. *)

(*
  -----------
  EXPRESSIONS
  -----------
*)

val add_key_eq_int : Concolic_key.t -> int -> t -> t
(** [add_key_eq_int k i t] adds the claim that [k = i] in [t]. *)

val add_key_eq_bool : Concolic_key.t -> bool -> t -> t
(** [add_key_eq_int k b t] adds the claim that [k = b] in [t]. *)

val add_alias : Concolic_key.t -> Concolic_key.t -> t -> t
(** [add_alias k k' t] adds the formula that [k = k'] in [t] where [k'] was defined first. *)

val add_binop : Concolic_key.t -> Expression.Untyped_binop.t -> Concolic_key.t -> Concolic_key.t -> t -> t
(** [add_binop x op left right t] adds the formula that [x = left op right] in [t]. *) 

val add_input : Concolic_key.t -> Dvalue.t -> t -> t
(** [add_input x v t] is [t] that knows input [x = v] was given. *)

val add_not : Concolic_key.t -> Concolic_key.t -> t -> t
(** [add_not x y t] adds [x = not y] to [t]. *)

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : Branch.Runtime.t -> t -> t
(** [hit_branch branch t] is [t] that knows [branch] has been hit during interpretation. *)

val found_assume : Concolic_key.t -> t -> t
(** [found_assume key t] tells [t] that [key] is assumed to be true. *)

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

val finish : (t, Path_tree.t -> Dead.t) Options.Fun.a
(** [finish t root] creates a finished session from [t] that merges info with the given [root].
    The merged result can be gotten with [root_exn @@ finish t root]. *)

val make : Target.t -> Expression.Cache.t -> Concolic_feeder.t -> t
(** [make target feeder] makes an empty t that knows the given [target] and [feeder]. *)
