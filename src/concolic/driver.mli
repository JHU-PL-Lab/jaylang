
open Common

module type S = sig
  type tape
  module type DRIVER = sig
    val test_some_program :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Lang.Ast.some_program ->
      Status.Terminal.t * tape
    (** Performs concolic evaluation on the provided program or times out if the
        timeout limit was exceeded.  The result is printed to stdout. *)

    val test_some_file :
      options:Options.t ->
      do_wrap:bool ->
      do_type_splay:Translate.Splay.t ->
      Core.Filename.t ->
      Status.Terminal.t * tape
    (** Performs concolic evaluation on the program in the provided file or times
        out if the timeout limit was exceeded.  The result is printed to stdout. *)

    val eval : Status.Terminal.t Cmdliner.Cmd.t
    (** [eval] can be run with [Cmdliner] to run [test] on the command line arguments. *)
  end

  (* This is generative because a new solver contexts are made *)
  module Make (Key : Smt.Symbol.KEY) (_ : Target_queue.MAKE) (_ : Evaluator.EVAL with type k := Key.t) () : DRIVER

  module Eager : DRIVER

  module Deferred : DRIVER

  module Default = Eager

  include DRIVER (* Is Default *)
end

(* No logging -- anything logged is just ignored, and the final tape is () *)
include S with type tape = unit

module Of_logger (T : Utils.Logger.TRANSFORMER with type B.a = Stat.t) : S with type tape = T.tape
