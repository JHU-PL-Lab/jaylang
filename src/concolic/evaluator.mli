
open Common

type 'k eval = Lang.Ast.Embedded.t -> 'k Interp_common.Input_feeder.t ->
  max_step:Interp_common.Step.t -> Status.Eval.t * 'k Path.t
(** ['k eval] is the type of functions that concolically evaluate programs to a path
    of concolic formulas keyed by ['k]. *)

module type EVAL = sig
  type k
  val ceval : k eval
end

(*
  Here, we have passed in a logger that has already been transformed with the P monad.
  This is in contrast to how we pass in a key and a functor to make a target queue from that key.
  The parallel would be to pass in a log transformer.
  (But I should just pass in a made target queue and pull the k from that!)
*)
module Make : functor (K : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (P : Pause.S) (Log : Utils.Logger.FULL with type B.a = Stat.t and type 'a M.m = 'a P.m) -> sig
  val c_loop : options:Options.t -> K.t eval -> K.t Smt.Formula.solver -> Lang.Ast.Embedded.t -> Status.Terminal.t Log.m
  (** [c_loop ~options eval pgm] is the result of concolic looping on [pgm] using the concolic
      evaluation function [eval]. *)
end
