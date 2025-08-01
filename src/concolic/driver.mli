(**
   File: driver.mli
   Purpose: the external interface for starting up the concolic evaluator

   Detailed description:
    The driver runs the concolic evaluator on either filenames or
    already-parsed Bluejay programs. The translator is run according
    to the [do_wrap] named argument, as well as with consideration for
    all of the provided optional arguments.
*)

open Concolic_common

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

val ceval : Concolic_common.Status.Terminal.t Cmdliner.Cmd.t
(** [ceval] can be run with [Cmdliner] to run [test] on the command line arguments. *)
