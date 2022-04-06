open Core
open Dbmc

let usage_msg = "odefa -i <file> [<input_i>]"
let source_file = ref ""
let inputs = ref []

let anon_fun i_raw =
  let this_i = Int.of_string i_raw in
  inputs := !inputs @ [ this_i ]

let run_program source =
  let program = File_util.read_source source in
  let session = Interpreter.create_interpreter_session !inputs in
  try Interpreter.eval session program with
  | Interpreter.Terminate v -> Format.printf "%a" Odefa_ast.Ast_pp.pp_value v
  | ex -> raise ex

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Iutput source file") ]
    anon_fun usage_msg ;
  run_program !source_file
