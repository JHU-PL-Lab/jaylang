
exception NoParentException
(** thrown to convey that the tracker does not have the parent it expects *)

type t
(** [t] tracks expressions during the evaluation of a Jayil program. It has a functional
    state that is the current environment of the Jayil program; it is underneath some
    branch in the program or is in the global scope. *)

val empty : t
(** [empty] is a tracker in the global environment with no formulas. *)

val enter_branch : t -> Branch.Runtime.t -> t
(** [enter_branch t b] puts the tracker [t] in the environment under branch [b]. *)

val exit_branch : t -> t
(** [exit_branch t] backs the tracker [t] out of its branch into the parent branch. *)

val add_key_eq_val : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_key_eq_val t k v] adds the formula that [k] has value [v] in tracker [t]. *)

val add_alias : t -> Lookup_key.t -> Lookup_key.t -> t
(** [add_alias t k k'] adds the formula that [k] and [k'] hold the same value in tracker [t]. *)

val add_binop : t -> Lookup_key.t -> Jayil.Ast.binary_operator -> Lookup_key.t -> Lookup_key.t -> t
(** [add_binop t x op left right] adds the formula that [x = left op right] to the tracker [t]. *)

val add_input : t -> Lookup_key.t -> Jayil.Ast.value -> t
(** [add_input t x v] is a tracker that knows the input [x = v] was given. *)

val add_formula : t -> Z3.Expr.expr -> t
(** [add_formula t e] adds the z3 expression [e] to the tracker [t]. *)

val is_global : t -> bool
(** [is_global t] is true if and only if the tracker [t] is in the global scope. *)

val exit_until_global : t -> t
(** [exit_until_global t] brings [t] back to the global scope. *)

val union : t -> t -> t option
(** [union a b] is all info from [a] and [b] if both are global. Otherwise is None. *)

val all_formulas : t -> target:Branch.t -> aborts:Branch.t list -> max_steps:Branch.t list -> Z3.Expr.expr list
(** [all_formulas t target aborts max_steps] is all formulas in [t] that are known to solve for the [target] AST
    branch, along with formulas that set any branches given in [aborts] or [max_steps] to be off limits so
    that the branches are not to be hit during the run for the [target]. *)