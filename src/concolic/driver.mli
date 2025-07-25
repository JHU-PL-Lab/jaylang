(**
  File: driver.mli
  Purpose: the external interface for starting up the concolic evaluator

  Detailed description:
    The driver runs the concolic evaluator on either filenames or
    already-parsed Bluejay programs. The translator is run according
    to the [do_wrap] named argument, as well as with consideration for
    all of the provided optional arguments.

  Dependencies:
    Basically everything in the `concolic/` directory.
*)


type 'a test = ('a, do_wrap:bool -> do_type_splay:bool -> Concolic_common.Status.Terminal.t) Concolic_common.Options.Arrow.t

val test_bjy : Lang.Ast.Bluejay.pgm test
(** [test_bjy pgm] is the result of concolic evaluation on [pgm],
    or timeout if the timeout limit was exceeded. The result is printed
    to stdout. *)

val test : Core.Filename.t test
(** [test filename do_wrap] is the result of concolic evaluation on the Bluejay
    program parsed from [filename], or timeout if the timeout limit was
    exceeded. The result is printed to stdout. *)

val ceval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
(** [ceval] can be run with [Cmdliner] to run [test] on the command line arguments. *)
