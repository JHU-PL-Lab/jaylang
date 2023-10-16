open Core
open Dbmc

let usage_msg = "jil -i <file> [<input_i>]"
let source_file = ref ""
let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let run_program source =
  let open Jil_analysis in
  let program = Dj_common.File_utils.read_source source in
  let session =
    {
      (Interpreter.make_default_session ()) with
      input_feeder = Input_feeder.from_list !inputs;
    }
  in
  try Interpreter.eval session program with
  | Interpreter.Terminate v -> Format.printf "%a" Interpreter.pp_dvalue v
  | ex -> raise ex

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Iutput source file") ]
    anon_fun usage_msg ;

  run_program !source_file
