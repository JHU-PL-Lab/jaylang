(**
   This module defines a monad used by the symbolic interpreter.  This monad
   provides the following computational properties:

   * Non-determinism (for choices on e.g. wiring nodes)
   * State within (rather than across) non-determinism (to track decisions and
     constraints)
   * SAT checks on collected constraints
   * Stepping incrementally through computation

   To support incremental stepping, this monad's data type is essentially a free
   monad: binds and returns both produce suspended structures with sufficient
   metadata to support the other features.  This monad also provides an
   evaluation stepping routine to allow it to be incrementally evaluated.
*)

open Batteries;;

open Odefa_ast;;

open Ast;;
open Interpreter_types;;
(* open Relative_stack;; *)
open Sat_types;;

(* **** Monadic interface **** *)

(** The type describing a computation in this monad.  This type does not include
    a state; it is waiting to be run. *)
type 'a m;;

(** The return operation. *)
val return : 'a -> 'a m;;

(** The bind operation. *)
val bind : 'a m -> ('a -> 'b m) -> 'b m;;

(** A zero operation.  Produces an empty value containing no computations. *)
val zero : unit -> 'a m;;

(** Non-deterministic selection. *)
val pick : 'a Enum.t -> 'a m;;

(** Computational suspension.  When stepping the evaluation of a monadic value,
    invocations of this function will mark the completion of a step. *)
val pause : unit -> unit m

(** Records a path decision for the provided variable.  If the provided search
    path is valid in this environment, unit is returned in the environment.  If
    the provided search path conflicts with a previous selection, the
    environment is destroyed.  The first symbol is the variable which is being
    mapped; the remaining arguments are the components of the wiring node. *)
val record_decision : Symbol.t -> Ident.t -> clause -> Ident.t -> unit m;;

(** Stores a formula in this environment's constraint set. *)
val record_formula : Formula.t -> unit m;;

(** Verifies that the formulae in this environment are solvable. *)
val check_formulae : unit -> unit m;;

(* **** Evaluation interface **** *)

(** The type describing a running evaluation of a computation in this monad.
    This evaluation is either completed (in which case it is simply a value and
    its associated state) or it is suspended (in which case it is a monadic
    computation and its associated state). *)
type 'a evaluation;;

(** Initializes a computation in this monad.  This is similar to the "run"
    routines of standard monads except that it does not run to completion; it
    runs to the first pause. *)
val start : 'a m -> 'a evaluation;;

(** Performs one step of evaluation.  As evaluation is non-deterministic,
    multiple evaluations (either suspended or completed) may be returned.
    Stepping a completed evaluation is a no-op. *)
val step : 'a evaluation -> 'a evaluation Enum.t;;

(** Retrieves the current formulae from an evaluation. *)
val get_formulae : 'a evaluation -> Formulae.t;;

(** Retrieves the result of an evaluation.  If the evaluation is suspended and
    still requires stepping to complete, None is returned. *)
val get_result : 'a evaluation -> 'a option;;
