open Core
open From_dbmc

let usage_msg = "jil -i <file> [<input_i>]"
(* let source_file = ref "" *)
let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let run_program source =
  let program = Dj_common.File_utils.read_source source in
  let session =
    {
      (Interpreter.make_default_session ()) with
      input_feeder = Dj_common.Input_feeder.from_list !inputs;
    }
  in
  try Interpreter.eval session program with
  | Interpreter.Terminate v -> Format.printf "%a" Interpreter.pp_dvalue v
  | ex -> raise ex

let () =
  match Sys.get_argv () |> List.of_array with
  | _ :: "-i" :: source_file :: inputs ->
    List.iter inputs ~f:anon_fun;
    run_program source_file
  | _ -> Format.printf "error in parsing. Usage message: %s\n" usage_msg
