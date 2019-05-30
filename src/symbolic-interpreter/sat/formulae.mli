(**
   This module defines a data structure for storing and analyzing logical
   formulae regarding a symbolically interpreted program.
*)

open Batteries;;
open Odefa_ast;;

open Ast;;
open Interpreter_types;;
open Sat_types;;

(** A type representing the kind of information a formula symbol contains. *)
type symbol_type =
  | IntSymbol
  | BoolSymbol
  | FunctionSymbol of function_value
;;

(** An exception which is raised if an inconsistent formula is added to a
    collection.  This is not guaranteed to be thrown in all contradictory
    circumstances; it is thrown when the intersection of types assigned to a
    symbol produces the empty set. *)
exception SymbolTypeContradiction of string * symbol * symbol_type list;;

(** A type representing a collection of formulae. *)
type t;;

(** An empty collection of formulae. *)
val empty : t;;

(** Adds an element to a collection.  May raise a SymbolTypeContradiction if the
    formula eliminates all possible values for a symbol. *)
val add : formula -> t -> t;;

(** Creates a singleton collection. *)
val singleton : formula -> t;;

(** Unions two collections.  May raise a SymbolTypeContradiction as in the add
    function. *)
val union : t -> t -> t

(** Enumerates the formulae in a collection. *)
val enum : t -> formula Enum.t;;

(** Processes each formula in a collection. *)
val iter : (formula -> unit) -> t -> unit;;

(** Retrieves the inferred type of a symbol in a collection.  If None is
    returned, the symbol has no fixed type; this may occur if a symbol has been
    introduced via a formula but has never been constrained.  Raises Not_found
    if the symbol has never been introduced. *)
val type_of : symbol -> t -> symbol_type option;;
