(**
   This module is meant to test reachability in a push-down system which accepts
   by empty stack.
*)

module type Basis = Pds_reachability_basis.Basis;;
module Make = Pds_reachability_analysis.Make;;
