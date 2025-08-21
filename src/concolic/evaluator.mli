
open Concolic_common

type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t ->
  max_step:Interp_common.Step.t -> Status.Eval.t * 'k Path.t
(** ['k eval] is the type of functions that concolically evaluate programs to a path
    made of concolic formulas keyed by ['k]. *)

module type EVAL = sig
  type k
  val ceval : k eval
end

module Make : functor (K : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (P : Pause.S) (Log_t : Stat.LOG_T) -> sig
  val c_loop : options:Options.t -> K.t eval -> K.t Smt.Formula.solver -> Lang.Ast.Embedded.t -> Concolic_common.Status.Terminal.t Log_t(P).m
  (** [c_loop ~options eval pgm] is the result of concolic looping on [pgm] using the concolic
      evaluation function [eval]. *)
end
