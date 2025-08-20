
open Concolic_common

module type S = sig
  val test_some_program :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:bool ->
    Lang.Ast.some_program ->
    Concolic_common.Status.Terminal.t
  (** Performs concolic evaluation on the provided program or times out if the
      timeout limit was exceeded.  The result is printed to stdout. *)

  val test_some_file :
    options:Options.t ->
    do_wrap:bool ->
    do_type_splay:bool ->
    Core.Filename.t ->
    Concolic_common.Status.Terminal.t
  (** Performs concolic evaluation on the program in the provided file or times
      out if the timeout limit was exceeded.  The result is printed to stdout. *)

  val eval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
  (** [eval] can be run with [Cmdliner] to run [test] on the command line arguments. *)
end

(* This is generative because a new solver contexts are made *)
module Make (Key : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (_ : Evaluator.EVAL with type k := Key.t) (_ : Stat.LOG_T) () : S

module Eager : S

module Deferred : S

module Default = Eager

include S (* Is Default *)
