open Batteries;;
open Jhupllib;;

open Odefa_ast;;
open Odefa_test_generation;;

let logger = Logger_utils.make_logger "Test_generator";;
let lazy_logger = Logger_utils.make_lazy_logger "Test_generator";;

exception CommandLineParseFailure of string;;

let () =
  (* Parse CLI args *)
  let args = Generator_configuration_parser.parse_args () in
  (* Read the AST *)
  let ast =
    try
      File.with_file_in args.ga_filename Odefa_parser.Parser.parse_program
    with
    | Sys_error err ->
      prerr_endline err;
      exit 1
  in
  (* Check well-formedness of AST *)
  begin
    try
      Ast_wellformedness.check_wellformed_expr ast;
    with
    | Ast_wellformedness.Illformedness_found ills ->
      begin
        print_endline "Program is ill-formed.";
        ills
        |> List.iter
          (fun ill ->
             print_string "* ";
             print_endline @@ Ast_wellformedness.show_illformedness ill;
          );
        ignore @@ Pervasives.exit 1
      end;
  end;
  (* Generate tests *)
  try
    let generator =
      Generator.create args.ga_generator_configuration ast args.ga_target_point
    in
    let generation_callback (inputs : int list) (steps : int) : unit =
      Printf.printf "Input sequence: [%s]\nGenerated in %d steps.\n\n"
        (String.join ", " @@ List.map string_of_int inputs) steps;
      flush stdout;
    in
    let answers, generator_opt =
      Generator.generate_inputs
        ~generation_callback:generation_callback
        args.ga_maximum_steps
        generator
    in
    let answer_count = List.length answers in
    Printf.printf "%d answer%s generated\n"
      answer_count (if answer_count = 1 then "" else "s");
    if Option.is_none generator_opt then
      print_endline "No further answers exist."
    else
      print_endline "Further answers may exist."
  with
  | Odefa_symbolic_interpreter.Interpreter.Invalid_query msg ->
    prerr_endline msg
;;
