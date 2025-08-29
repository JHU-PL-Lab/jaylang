open Cmdliner
open Cmdliner.Term.Syntax

(*
  Give input sequences to the deferred interpreter like this:

    ./deval.exe ./filename.bjy --inputs '1 1 2 3 -100 true false 10 true'

  It is important that the types are correct in the order of inputs. If in the
  above, the 6th input the interpreter asks for is actually an int, then a default
  integer will be given; the program will not break nor issue a warning because
  the user gave `true` as the 6th input.

  Carrying on from that, if the 7th input needed is indeed a boolean, then it
  will be given `true` from the list; inputs carry on as normal after the type-failing
  one.
*)

let deval =
  Cmd.v (Cmd.info "deval") @@
  let+ pgm = Lang.Parser.parse_program_from_argv 
  and+ `Do_wrap do_wrap, `Do_type_splay do_type_splay = Translate.Convert.cmd_arg_term
  and+ inputs = Interp_common.Input.parse_list in
  Translate.Convert.some_program_to_emb ~do_wrap ~do_type_splay pgm
  |> Deferred.Main.deval_with_input_sequence inputs
  |> (function
      | Ok v -> Format.printf "Your program evaluated to:\n%s\n" (Deferred.Value.Without_symbols.to_string v)
      | Error e -> Format.printf "ERROR: the deferred interpreter did not evaluate to a value:\n%s\n" (Deferred.Err.to_string e)
    )

let () = 
  exit @@ Cmd.eval deval
