(**
  File: z3_intf.mli
  Purpose: an interface to Z3 for the concolic evaluator

  Detailed description:
    The concolic evaluator uses the Z3 SMT solver to solve constraints.
    It needs only integer and boolean symbolic expressions, as well
    as some binary operations. That is what is supported here.

    Expressions can be built, identified with stepkeys, and solved for
    through this interface.

    Much of this behavior comes from Utils.Z3_api, which includes
    everything that is independent of concolic evaluation (e.g.
    it includes all except the stepkeys).

    The behavior here is used by [Solve] and [Expression] in a
    sort of layered architecture. Any solving done by the concolic
    evaluator is through those modules.

  Dependencies:
    Stepkey -- identifies the symbolic variables

*)

open Core

module type S = sig
  type 'a t (* expressions *)

  val set_timeout : Time_float.Span.t -> unit

  (* val ctx : Z3.context *)

  (*
    -------------
    MAKE FORMULAS
    -------------
  *)
  val box_int : int -> int t
  val box_bool : bool -> bool t
  
  val var_of_key : 'a Stepkey.t -> 'a t

  (*
    ------------------
    VALUES OF FORMULAS 
    ------------------
  *)
  val value_of_key : Z3.Model.model -> 'a Stepkey.t -> 'a option

  (*
    ----------------
    COMBINE FORMULAS
    ----------------
  *)
  val not_ : bool t -> bool t
  val plus : int t -> int t -> int t
  val minus : int t -> int t -> int t
  val times : int t -> int t -> int t
  val divide : int t -> int t -> int t
  val modulus : int t -> int t -> int t
  val less_than : int t -> int t -> bool t
  val less_than_eq : int t -> int t -> bool t
  val eq_ints : int t -> int t -> bool t
  val eq_bools : bool t -> bool t -> bool t
  val neq : int t -> int t -> bool t
  val and_ : bool t -> bool t -> bool t
  val or_ : bool t -> bool t -> bool t

  (*
    -----
    SOLVE
    -----
  *)
  module Solve_status : sig
    type t =
      | Sat of Z3.Model.model
      | Unknown
      | Unsat
  end

  val solve : bool t list -> Solve_status.t
end

module Make () : S