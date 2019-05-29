(**
   This module provides a rudimentary mechanism for typechecking formulae for
   the satisfiability engine.
*)

open Interpreter_types;;
open Sat_types;;

(** Describes the type of symbols in the solver. *)
type symbol_type =
  | IntSymbol
  | BoolSymbol
;;

(** This exception is raised if type inference fails.  The string is a
    human-readable message describing the problem.  The list of types indicate
    all of the types to which the symbol was expected to be equal; None values
    represent types invalid to the solver. *)
exception FormulaTypeError of string * symbol * symbol_type option list;;

(**
   Given a set of formulae, this function produces a mapping from symbols in the
   formulae to a symbol type.  These symbols should be explicitly declared to
   have their correpsonding types in the solver.  If a symbol is missing from
   this dictionary, neither its type nor any equations in which it appears
   should be presented to the solver (because they are not checkable by the
   solver due to e.g. being a non-solver type such as a record or e.g. being
   unconstrained).  If an error occurs, FormulaTypeError is raised.
*)
val infer_types : Formula_set.t -> symbol_type Symbol_map.t;;
