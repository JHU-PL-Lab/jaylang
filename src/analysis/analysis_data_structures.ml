(**
  Defines basic data structures for the analysis.  This module does not contain
  CBA-specific structures; instead, it merely contains functor-generated data
  structures for Odefa AST elements.
*)

open Batteries;;

open Ast;;

module Clause_ord =
struct
  type t = clause;;
  let compare = compare_clause;;
end;;

module Clause_set = Set.Make(Clause_ord);;
