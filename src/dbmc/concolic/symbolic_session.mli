open Path_tree

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

val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in [t]. *)

val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in [t]. *)

val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] in [t]. *)

val add_input : t -> Lookup_key.t -> Dvalue.t -> t
(** [add_input t x v] is [t] that knows input [x = v] was given. *)

val add_not : t -> Lookup_key.t -> Lookup_key.t -> t
(** [add_not t x y] adds [x = not y] to [t]. *)

val add_match : t -> Lookup_key.t -> Lookup_key.t -> Jayil.Ast.pattern -> t
(** [add_match t x y pat] adds [x = y ~ pat] to [t]. *)

(*
  -----------------
  CONTROL FLOW ETC.
  -----------------
*)

val hit_branch : t -> Branch.Runtime.t -> t
(** [hit_branch t branch] is [t] that knows [branch] has been hit during interpretation. *)

val fail_assume : t -> Lookup_key.t -> t
(** [fail_assume t key] tells [t] that the variable in [key] was false when it was assumed to be true. *)

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
val next : t -> Target.t -> t (* TODO: remove *)
val make : Root.t -> Target.t -> t

val root_exn : t -> Root.t
val targets_exn : t -> Target.t list
val branch_info : t -> Branch_info.t
val hit_max_depth : t -> bool