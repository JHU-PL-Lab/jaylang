open Cmdliner
open Cmdliner.Term.Syntax

let deval =
  Cmd.v (Cmd.info "deval") @@
  let+ pgm = Lang.Parse.parse_bjy_file_from_argv 
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term in
  Translate.Convert.bjy_to_emb pgm ~do_wrap ~do_type_splay
  |> Lang.Ast_tools.Utils.pgm_to_module
  |> Lang.Ast.Embedded.With_program_points.t_of_expr
  |> Deferred.Main.deval
  |> function
    | Ok v -> Format.printf "Your program evaluated to:\n%s\n" (Deferred.Value.Without_symbols.to_string v)
    | Error e -> Format.printf "ERROR: the deferred interpreter hit an error:\n%s\n" (Deferred.Err.to_string e)

let () = 
  exit @@ Cmd.eval deval
