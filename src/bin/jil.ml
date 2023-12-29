open Core
open Dbmc

let usage_msg = "jil -i <file> [<input_i>]"
let source_file = ref ""
let inputs = ref []

let mode = ref ""

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


(* Run concolic tester: *)
let rec test_program_concolic source counter =
  if counter <= 0 then Format.printf "Timeout limit reached. Program will not terminate...\n" else 
  let program = Dj_common.File_utils.read_source source in 
  try
    let _ = Concolic.eval program in ()
  with
  | Concolic_exceptions.Reach_max_step(_, _, _) -> Format.printf "Reach max step... re-evaluating:\n"; test_program_concolic source (counter - 1)
  | ex -> raise ex 

let speclist = 
  [("-i", Arg.Set_string source_file, "Input source file");
    ("-m", Arg.Set_string mode, "Interpreter mode")]

let reset_limit = 20

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !mode with 
  | "concolic" -> test_program_concolic !source_file reset_limit
  | "normal" -> run_program !source_file
  | _ -> ()