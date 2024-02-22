
type t
(** [t] tracks paths in the program during and between interpretations of the program.
    It holds symbolic formulas to solve to hit other paths. *)

(*
  --------
  CREATION   
  --------
*)

val empty : t
(** [empty] is a default path tracker with no target and empty tree and stack. *)

val with_options : (t -> t) Concolic_options.Fun.t

val of_expr : Jayil.Ast.expr -> t
(** [of_expr expr] is [empty] that knows of all branches in the [expr]. *)

(*
  ------------------
  RUNTIME OPERATIONS   
  ------------------
*)
(* val add_formula : t -> Z3.Expr.expr -> t *)
(** [add_formula t expr] adds [expr] to the node in [t] that corresponds to the current location in interpretation. *)
(* ^ no longer needed because the functions below handle all expressions. Only keys and values need to be handled alongside the interpreter. *)

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

val hit_branch : t -> Branch.Runtime.t -> t
(** [hit_branch t branch] is [t] that knows [branch] has been hit during interpretation. *)

val fail_assume : t -> Lookup_key.t -> t
(** [fail_assume t key] tells [t] that the variable in [key] was false when it was assumed to be true. *)

val found_abort : t -> t
(** [found_abort t] tells [t] that an abort was found in interpretation. *)

val reach_max_step : t -> t
(** [reach_max_step t] tells [t] that the max interpretation step was hit, and interpretation stopped. *)
(*
  ----------------------
  BETWEEN-RUN OPERATIONS   
  ----------------------
*)

val next : t -> [ `Done of Branch_tracker.Status_store.Without_payload.t | `Next of (t * Session.Eval.t) ]
(** [next t] is a path tracker intended to hit the most prioritized target after the run in [t]. *)

val status_store : t -> Branch_tracker.Status_store.Without_payload.t
(** [status_store t] is the status store in [t]. *)

val run_num : t -> int
(** [run_num t] is the number of interpretations [t] has done. *)