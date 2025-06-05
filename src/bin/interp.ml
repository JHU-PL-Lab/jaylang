(**
  Bin [interp].

  This executable runs the interpreter on the program at the
  given file path. The file must be a Bluejay (`.bjy`) file,
  and the mode (`-m`) argument allows the user to translate the
  programs to other languages before interpreting.
*)

open Cmdliner
open Cmdliner.Term.Syntax

let interp =
  Cmd.v (Cmd.info "interp") @@
  let+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ pgm = Lang.Parse.parse_bjy_file_from_argv
  and+ mode = Arg.(value
    & opt (enum ["bluejay", `Bluejay; "desugared", `Desugared; "embedded", `Embedded]) `Bluejay
    & info ["m"] ~doc:"Mode: bluejay, desugared, or embedded. Default is bluejay.")
  in
  match mode with
  | `Bluejay -> Core.Fn.const () @@
    Lang.Interp.eval_pgm pgm
  | `Desugared -> Core.Fn.const () @@
    (* Type splaying not allowed if the program is being interpretted in desugared mode *)
    Lang.Interp.eval_pgm @@ Translate.Convert.bjy_to_des pgm ~do_type_splay:false
  | `Embedded -> Core.Fn.const () @@
    Lang.Interp.eval_pgm @@ Translate.Convert.bjy_to_emb pgm ~do_wrap ~do_type_splay

let () = 
  exit @@ Cmd.eval interp
