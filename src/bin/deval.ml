open Cmdliner
open Cmdliner.Term.Syntax

let deval =
  Cmd.v (Cmd.info "deval") @@
  let+ pgm = Lang.Parse.parse_bjy_file_from_argv 
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term in
  Translate.Convert.bjy_to_emb pgm ~do_wrap ~do_type_splay
  |> Lang.Ast_tools.Utils.pgm_to_module
  |> Lang.Ast.Embedded.With_program_points.t_of_expr
  |> Deferred.Main.stern_eval
  |> Deferred.Effects.run_on_empty
  |> function
    | Ok _v, _ -> Format.printf "Your program evaluated to a value\n"
    | Error _e, _ -> Format.printf "ERROR: the deferred interpreter hit an error.\n" 

let () = 
  exit @@ Cmd.eval deval
