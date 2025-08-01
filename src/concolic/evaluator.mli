(**
   File: evaluator.mli
   Purpose: concolically evaluate the program repeatedly

   Detailed description:
    This runs the interpreter repeatedly and only stops when at a terminal
    status--timeout was hit, a program error was found, or all paths were
    exhausted.
*)

open Concolic_common

val global_runtime : float Utils.Safe_cell.t
(** [global_runtime] is a cell containing the total global time spent
    on interpretation. *)

val global_solvetime : float Utils.Safe_cell.t
(** [global_solvetime] is a cell containing the total global time spent
    on solving between interpretations. This inludes any management of
    symbolic constraints; it is not strictly the time spent using Z3 to solve. *)

type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t -> 
  max_step:Interp_common.Step.t -> Concolic_common.Status.Eval.t * 'k Concolic_common.Path.t
(** ['k eval] is the type of functions that concolically evaluate programs to a path
    made of concolic formulas keyed by ['k]. *)

val eager_eval : Interp_common.Step.t eval
(** [eager_eval] is an eager concolic evaluator with standard eager semantics. *)

module Make : functor (K : Smt.Symbol.KEY) (_ : Target_queue.Make(K).S)
  (_ : Smt.Formula.SOLVABLE) (P : Pause.S) -> sig
  val c_loop :
    options:Options.t ->
    K.t eval ->
    Lang.Ast.Embedded.t ->
    Concolic_common.Status.Terminal.t P.t
    (** [c_loop eval pgm] is the result of concolic looping on [pgm] using the concolic
        evaluation function [eval]. *)
end

val eager_c_loop :
  options:Options.t ->
  Lang.Ast.Embedded.t -> Concolic_common.Status.Terminal.t Lwt.t
(** [eager_c_loop pgm] is the result of concolic evaluation on [pgm] using the default
    global [Solve.S] module and eager evaluation. *)
