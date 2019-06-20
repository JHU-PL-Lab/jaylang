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
open Sat_types;;

(** The specification of a symbolic monad. *)
module type Spec = sig
  (* A definition of the type used as a caching key. *)
  module Cache_key : Interfaces.OrderedType;;
end;;

(** The interface of a symbolic monad. *)
module type S = sig
  (** The specification for this symbolic monad. *)
  module Spec : Spec;;

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

  (** Checks that a monadic value's formulae are solvable.  For each
      non-deterministic path in the provided monadic value, a solution is
      attempted for the formulae.  Any paths with unsolvable formulae are zeroed
      out; the remaining paths exist in the returned monadic value (if any). *)
  val check_formulae : 'a m -> 'a m;;

  (* **** Evaluation interface **** *)

  (** The type describing a running evaluation of a computation in this monad.
      This value represents all non-deterministic outcomes of a monadic value.
      As an evaluation is stepped, it produces completed values and may or may
      not terminate. *)
  type 'a evaluation;;

  (** A type describing a single evaluation result. *)
  type 'a evaluation_result =
    { er_value : 'a;
      er_formulae : Formulae.t;
      er_steps : int;
    };;

  (** Initializes a computation in this monad.  This is similar to the "run"
      routines of standard monads except that it does not run to completion; it
      runs to the first pause. *)
  val start : 'a m -> 'a evaluation;;

  (** Performs one step of evaluation.  Evaluation continues until all
      non-deterministic outcomes are paused; these paused outcomes are
      represented by the returned evaluation.  The returned enumeration contains
      all completed values discovered in this step together with the formulae
      they induced. *)
  val step : 'a evaluation -> 'a evaluation_result Enum.t * 'a evaluation;;

  (** Determines whether a particular evaluation has completed.  If so,
      stepping the evaluation will never produce additional results.  Note that
      false does not guarantee that further values exist to be produced; it
      only indicates that evaluation is not yet finished. *)
  val is_complete : 'a evaluation -> bool;;
end;;

(** The interface of the functor producing symbolic monads. *)
module Make(Spec : Spec) : S with module Spec = Spec;;
