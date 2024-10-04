
(*
  IMPORTANT:
    This module uses internal state and is not thread-safe.
*)

open Core

module Solve_status :
  sig
    type t =
      | Sat of Z3.Model.model
      | Unknown
      | Unsat
  end

val set_timeout : Time_float.Span.t -> unit
(** [set_timeout time] sets the timeout for a single solve. *)

val solve : Z3.Expr.expr list -> Solve_status.t
(** [solve exprs] tries to get a statisfying model for the given expressions [exprs] and returns the status. *)

val get_int_expr : Z3.Model.model -> Concolic_key.t -> int option
(** [get_int_expr model key] gets the integer that should be input for the clause identified by [key]
    from the given [model], or none if the clause is not known to the model. *)

(*
  --------------
  AST PRIMITIVES
  --------------
*)

val not_ : Concolic_key.t -> Concolic_key.t -> bool Sudu.Gadt_z3_api.Gexpr.t
(** [not x y] is the expression that [x = not y]. *)

val binop : Concolic_key.t -> Jayil.Ast.binary_operator -> Concolic_key.t -> Dvalue.t -> Concolic_key.t -> bool Sudu.Gadt_z3_api.Gexpr.t
(** [binop x op y left_v z] is the expression that [x = op y z] with prefix notation for the operator [op], where
    the concrete value for [y] is [left_v], which is a quick patch in order to help the solver know the sort
    in case [op] is equality. *)

val eqv : Concolic_key.t -> Jayil.Ast.value -> bool Sudu.Gadt_z3_api.Gexpr.t
(** [eqv x v] is the expression that [x = v]. *)

val eq : Dvalue.t -> Concolic_key.t -> Concolic_key.t -> bool Sudu.Gadt_z3_api.Gexpr.t
(** [eq x y] is the expression that [x = y], i.e. it says [x] is an alias for [y]. *)
