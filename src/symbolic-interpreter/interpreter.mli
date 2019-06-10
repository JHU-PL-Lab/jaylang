(**
   This module contains a definition of the DDSE symbolic interpreter.
*)

open Odefa_ast;;
open Odefa_ddpa;;

open Ast;;
open Ddpa_graph;;
open Interpreter_types;;

(** This type represents an in-progress demand-driven evaluation of an
    expression. *)
type evaluation;;

(** The result of an evaluation. *)
type evaluation_result = {
  er_formulae : Formulae.t;
  (** The formulae established during symbolic evaluation. *)

  er_stack : Ident.t list;
  (** The stack for the target variable in this evaluation. *)

  er_solution : (symbol -> value option);
  (** The solution to the formulae found by this evaluation. *)
};;

(** Raised if a query is invalid (e.g. a variable is requested for an expression
    which does not contain it.  The payload of this exception is a
    human-readable message explaining the problem. *)
exception Invalid_query of string;;

(** Starts a demand-driven evaluation of an expression at the provided program
    point (described by a variable).  The provided CFG must be complete with
    respect to the expression. *)
val start : ddpa_graph -> expr -> Ident.t -> evaluation;;

(** Takes a step of demand-driven evaluation.  This routine returns any
    evaluation results it encounters in this step (as nondeterminism may lead to
    many) and an optional evaluation (if more evaluation may reveal more
    results).  As demand-driven evaluation is undecidable, there is no guarantee
    that a given evaluation will ever produce a result or terminate regardless
    of the number of evaluation steps taken. *)
val step : evaluation -> evaluation_result list * evaluation option;;
