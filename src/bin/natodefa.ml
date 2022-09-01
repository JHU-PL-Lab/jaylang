open Core
open Odefa_natural

let usage_msg = "natodefa -i <file>"
let source_file = ref ""
let anon_fun _ = failwith "No anonymous argument allowed!"

let run_program source =
  let program = File_util.read_source source in
  try Interpreter.eval program with _ -> failwith "TBI!"
(* | Interpreter.Terminate v -> Format.printf "%a" Jayil.Ast_pp.pp_value v
   | ex -> raise ex *)

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Iutput source file") ]
    anon_fun usage_msg ;
  run_program !source_file
