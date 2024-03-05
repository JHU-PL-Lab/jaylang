open Path_tree

module Lazy_key :
  sig
    type t
    val to_key : t -> Lookup_key.t
    val make : Jayil.Ast.ident -> Dj_common.Concrete_stack.t -> t
  end

type t
(** [t] tracks symbolic representations of the program during interpretation. *)

val empty : t
(** [empty] is a default symbolic session. *)

val with_options : (t -> t) Concolic_options.Fun.t
(** [with_options t] is [t] configured with the optional arguments. *)

(*
  -----------
  EXPRESSIONS
  -----------
*)

val add_key_eq_val : t -> Lazy_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in [t]. *)

val add_alias : t -> Lazy_key.t -> Lazy_key.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in [t]. *)

val add_binop : t -> Lazy_key.t -> Jayil.Ast.binary_operator -> Lazy_key.t -> Lazy_key.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] in [t]. *)

val add_input : t -> Lazy_key.t -> Dvalue.t -> t
(** [add_input t x v] is [t] that knows input [x = v] was given. *)

val add_not : t -> Lazy_key.t -> Lazy_key.t -> t
(** [add_not t x y] adds [x = not y] to [t]. *)

val add_match : t -> Lazy_key.t -> Lazy_key.t -> Jayil.Ast.pattern -> t
(** [add_match t x y pat] adds [x = y ~ pat] to [t]. *)

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : t -> Branch.Runtime.t -> t
(** [hit_branch t branch] is [t] that knows [branch] has been hit during interpretation. *)

val found_assume : t -> Lazy_key.t -> t
(** [found_assume t key] tells [t] that [key] is assumed to be true. *)

val fail_assume : t -> t
(** [fail_assume t] tells [t] that a recent assume/assert was false when it needs to be true. *)

val found_abort : t -> t
(** [found_abort t] tells [t] that an abort was found in interpretation. *)

val reach_max_step : t -> t
(** [reach_max_step t] tells [t] that the max interpretation step was hit, and interpretation stopped. *)

(*
  -----------
  BETWEEN-RUN   
  -----------
*)

val finish : t -> Root.t -> t
(** [finish t root] creates a finished session from [t] that merges info with the given [root].
    The merged result can be gotten with [root_exn @@ finish t root]. *)

val make : Root.t -> Target.t -> t
(** [make root target] makes an empty t that knows the given [root] and [target]. *)

(*
  ---------
  ACCESSORS   
  ---------
*)

val root_exn : t -> Root.t
(** [root_exn t] is the root from the finished [t] or exn. *)

val targets_exn : t -> Target.t list
(** [targets_exn t] is the targets in finished [t] or exn. *)

val branch_info : t -> Branch_info.t
(** [branch_info t] is current branch info from [t]. *)

val hit_max_depth : t -> bool
(** [hit_max_depth t] is true iff [t] reached the max allowed tree depth. *)

val is_reach_max_step : t -> bool