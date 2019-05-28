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

(* open Odefa_ast;; *)

(* open Ast;; *)
(* open Interpreter_types;; *)
(* open Relative_stack;; *)
open Sat_types;;
open Symbolic_monad_types;;

(* **** Monadic interface **** *)

(** The type describing values in this monad.  A single value of this type may
    represent multiple non-deterministic computations. *)
type 'a m;;

(** A return operation. *)
val return : 'a -> 'a m;;

(** A bind operation.  Computation is suspended at each bind. *)
val bind : 'a m -> ('a -> 'b m) -> 'b m;;

(** A non-deterministic selection. *)
val pick : 'a Enum.t -> 'a m;;

(** Records a search path for the provided variable.  If the provided search
    path is valid in this environment, unit is returned in the environment.  If
    the provided search path conflicts with a previous selection, the
    environment is destroyed.  The first symbol is the variable which is being
    mapped; the remaining arguments are the components of the wiring node. *)
(* val record_search : Symbol.t -> Ident.t -> clause -> Ident.t -> unit m;; *)

(** Stores a formula in this environment's constraint set. *)
val record_formula : Formula.t -> unit m;;

(* TODO: support functions from the notation in Section 5.3 *)

(* **** Execution interface **** *)

(** Performs the next step of evaluation on all computations appearing within
    the provided monadic value.  Computations which are complete are unaffected.
    To determine if a computation is complete, use the [unpack] and [examine]
    functions. *)
val step : 'a m -> 'a m

(* **** Inspection interface **** *)

(** The type describing individual computations in this monad.  A single value
    of this type represents exactly one non-deterministic computation: it is in
    a single state, but it's next step of execution may produce multiple
    states. *)
type 'a computation;;

(** Transforms a monadic value into a collection of computations. *)
val unpack : 'a m -> 'a computation Enum.t;;

(** Transforms a collection of computations into a monadic value. *)
val pack : 'a computation Enum.t -> 'a m;;

(** Given a computation, determines its state. *)
val examine : 'a computation -> 'a computation_state;;
