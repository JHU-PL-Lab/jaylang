(**
  Module [Typed_z3].

  This provides a typed interface to the Z3 SMT solver,
  allowing integer and boolean operations.

  The module is parametrized over a stateful Z3 context,
  and each instance of the result of the applicative functor
  has its own (stateful) solver that is transiently used
  for solves.

  A generative functor is also provided that creates a new
  context.
*)

module type S = sig
  type 'a t (* expressions *)
  type model

  val set_timeout : Core.Time_float.Span.t -> unit
  (** [set_timeout t] sets the timeout for a single solve to [t]. *)

  (*
    -------------
    MAKE FORMULAS
    -------------
  *)
  val box_int : int -> int t
  (** [box_int i] is an expression for the constant int [i]. *)

  val box_bool : bool -> bool t
  (** [box_bool b] is an expression for the constant int [b]. *)

  val int_var : int -> int t
  (** [int_var id] is an integer variable identified by [id]. *)

  val bool_var : int -> bool t
  (** [bool_var id] is a boolean variable identified by [id]. *)

  (*
    ------------------
    VALUES OF FORMULAS 
    ------------------
  *)

  val value_of_expr : model -> 'a t -> 'a option
  (** [value_of_expr model e] queries the [model] for the OCaml value
      associated with [e]. *)

  val constrained_vars : model -> int list
  (** [constrained_vars model] is the list of identifiers in the model,
      which all must have been arguments to [int_var] or [bool_var] previously. *)

  (*
    ----------------
    COMBINE FORMULAS
    ----------------
  *)
  val not_ : bool t -> bool t
  val plus : int t -> int t -> int t
  val minus : int t -> int t -> int t
  val times : int t -> int t -> int t
  val divide : int t -> int t -> int t
  val modulus : int t -> int t -> int t
  val less_than : int t -> int t -> bool t
  val less_than_eq : int t -> int t -> bool t
  val eq_ints : int t -> int t -> bool t
  val eq_bools : bool t -> bool t -> bool t
  val neq : int t -> int t -> bool t
  val and_ : bool t -> bool t -> bool t
  val or_ : bool t -> bool t -> bool t

  (*
    -----
    SOLVE
    -----
  *)
  module Solve_status : sig
    type t =
      | Sat of model
      | Unknown
      | Unsat
  end

  val empty_model : model
  (** [empty_model] is the model of an empty solver. *)

  val global_solvetime : float Utils.Safe_cell.t
  (** [global_solvetime] is a cell containing the total global time spent
      on solving. *)

  val solve : bool t list -> Solve_status.t
  (** [solve exprs] invokes the [Z3] solver for a solution to the [exprs]. *)
end

module Make (_ : sig val ctx : Z3.context end) : S
(** [Make] uses the provided context for the Z3 formulas. *)

module New_context () : S
(** [New_context] is a generative functor that makes a new context. *)
