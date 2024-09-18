
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

val reset : unit -> unit
(** [reset ()] clears known record labels from the state. Call this before running a new program to free space. *)

val get_int_expr : Z3.Model.model -> Concolic_key.t -> int option
(** [get_int_expr model key] gets the integer that should be input for the clause identified by [key]
    from the given [model], or none if the clause is not known to the model. *)

(*
  --------------
  AST PRIMITIVES
  --------------
*)

val not_ : Concolic_key.t -> Concolic_key.t -> Z3.Expr.expr
(** [not x y] is the expression that [x = not y]. *)

val binop : Concolic_key.t -> Jayil.Ast.binary_operator -> Concolic_key.t -> Concolic_key.t -> Z3.Expr.expr
(** [binop x op y z] is the expression that [x = op y z] with prefix notation for the operator [op]. *)

val eqv : Concolic_key.t -> Jayil.Ast.value -> Z3.Expr.expr
(** [eqv x v] is the expression that [x = v]. *)

val eq : Concolic_key.t -> Concolic_key.t -> Z3.Expr.expr
(** [eq x y] is the expression that [x = y], i.e. it says [x] is an alias for [y]. *)

val if_pattern : Concolic_key.t -> Jayil.Ast.pattern -> Z3.Expr.expr
(** [if_pattern key pat] is an expression that is the result of checking that [key] has the pattern [pat]. *)

val match_ : Concolic_key.t -> Concolic_key.t -> Jayil.Ast.pattern -> Z3.Expr.expr
(** [match_ key matched_key pat] is an expression that says [key] is the result of matching [match_key] with pattern [pat]. *)