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
  and+ pgm = Lang.Parser.parse_program_from_argv
  and+ inputs = Interp_common.Input.parse_list
  and+ mode = Arg.(value
                   & opt (enum ["type-erased", `Type_erased; "bluejay", `Bluejay; "desugared", `Desugared; "embedded", `Embedded]) `Default
                   & info ["m"] ~doc:"Mode: bluejay, desugared, or embedded. Default is based upon input file and is type-erased for Bluejay programs.")
  in
  let feeder =
    match inputs with
    | [] -> None
    | ls -> Some (Interp_common.Input_feeder.of_sequence ls)
  in
  match pgm with
  | Lang.Ast.SomeProgram (BluejayLanguage, pgm) ->
    begin
      match mode with
      | `Type_erased | `Default -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder @@
        Translate.Convert.bjy_to_erased pgm
      | `Bluejay -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder pgm
      | `Desugared -> Core.Fn.const () @@
        (* Type splaying not allowed if the program is being interpretted in desugared mode *)
        Interpreter.Interp.eval_pgm ?feeder @@
        Translate.Convert.bjy_to_des pgm ~do_type_splay:false
      | `Embedded -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder @@
        Translate.Convert.bjy_to_emb pgm ~do_wrap ~do_type_splay
    end
  | Lang.Ast.SomeProgram (DesugaredLanguage, pgm) ->
    begin
      match mode with
      | `Bluejay | `Type_erased ->
        failwith "Invalid mode for a desugared program."
      | `Desugared | `Default -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder pgm
      | `Embedded -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder @@
        Translate.Convert.des_to_emb pgm ~do_wrap ~do_type_splay
    end
  | Lang.Ast.SomeProgram (EmbeddedLanguage, pgm) ->
    begin
      match mode with
      | `Bluejay | `Type_erased | `Desugared ->
        failwith "Invalid mode for an embedded program."
      | `Embedded | `Default -> Core.Fn.const () @@
        Interpreter.Interp.eval_pgm ?feeder pgm
    end

let () = 
  exit @@ Cmd.eval interp
