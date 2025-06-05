(**
  Bin [ceval].

  This executable runs the concolic evaluator on the Bluejay
  program at the given file path.
*)

open Cmdliner
open Cmdliner.Term.Syntax

let ceval =
  Cmd.v (Cmd.info "ceval") @@
  let+ concolic_args = Concolic.Options.cmd_arg_term
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ bjy_pgm = Lang.Parse.parse_bjy_file_from_argv in
  Core.Fn.const ()
  @@ Concolic.Options.Arrow.appl
      Concolic.Driver.test_bjy
      concolic_args
      bjy_pgm
      ~do_wrap
      ~do_type_splay

let () = 
  exit @@ Cmd.eval ceval
