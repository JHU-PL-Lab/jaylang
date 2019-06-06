open Batteries;;
open Jhupllib;;

open Odefa_test_generation;;

let logger = Logger_utils.make_logger "Test_generator";;
let lazy_logger = Logger_utils.make_lazy_logger "Test_generator";;

exception CommandLineParseFailure of string;;

let () =
  let args = Generator_configuration_parser.parse_args () in
  let ast =
    try
      File.with_file_in args.ga_filename Odefa_parser.Parser.parse_program
    with
    | Sys_error err ->
      prerr_endline err;
      exit 1
  in
  let generator =
    Generator.create args.ga_generator_configuration ast args.ga_target_point
  in
  let generation_callback (inputs : int list) (steps : int) : unit =
    Printf.printf "Input sequence: [%s]\nGenerated in %d steps.\n\n"
      (String.join ", " @@ List.map string_of_int inputs) steps;
    flush stdout;
  in
  ignore @@ Generator.generate_inputs
    ~generation_callback:generation_callback
    args.ga_maximum_steps
    generator
;;
