
(*
  Literal Z3 formulas with a solver.

  It is recommended to use Smt.Formula.t to build
  formulas instead, and then transform into Z3 formulas.
*)
module Make () : Smt.Formula.SOLVABLE

module Default : Smt.Formula.SOLVABLE

include Smt.Formula.SOLVABLE with type ('a, 'k) t = ('a, 'k) Default.t
