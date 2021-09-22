(* open Core *)

open Batteries
open Odefa_ast
open Odefa_test_generation

exception CommandLineParseFailure of string

exception GenComplete

exception GenerationComplete

let () =
  (* Parse CLI args *)
  let args = Generator_configuration_parser.parse_args () in
  (* Read the AST *)
  let ast =
    let is_natodefa = Filename.extension args.ga_filename = ".natodefa" in
    if is_natodefa then (
      try
        let natast =
          File.with_file_in args.ga_filename
            Odefa_natural.On_parse.parse_program
        in
        Odefa_natural.On_to_odefa.translate natast
      with Sys_error err ->
        prerr_endline err;
        exit 1)
    else
      try File.with_file_in args.ga_filename Odefa_parser.Parser.parse_program
      with Sys_error err ->
        prerr_endline err;
        exit 1
  in
  (* Check well-formedness of AST *)
  (try Ast_wellformedness.check_wellformed_expr ast
   with Ast_wellformedness.Illformedness_found ills ->
     print_endline "Program is ill-formed.";
     ills
     |> List.iter (fun ill ->
            print_string "* ";
            print_endline @@ Ast_wellformedness.show_illformedness ill);
     ignore @@ Stdlib.exit 1);
  (* Generate tests *)
  try
    (* DBMC related code - start *)
    let config =
      {
        Dbmc.Top_config.default_config with
        filename = args.ga_filename;
        log_level = Some Logs.Debug;
        debug_lookup_graph = false;
      }
    in
    Dbmc.Log.init ~testname:args.ga_filename ~log_level:Logs.Debug ();
    let inputs = Dbmc.Main.lookup_main ~config ast args.ga_target_point in
    let inputs = List.hd inputs in
    Printf.printf "[%s]\n"
      (String.join ","
      @@ List.map (function Some i -> string_of_int i | None -> "_") inputs);
    Dbmc.Log.close ();
    ignore @@ raise GenComplete;

    (* DBMC related code - end *)
    let results_remaining = ref args.ga_maximum_results in
    let generator =
      Generator.create ~exploration_policy:args.ga_exploration_policy
        args.ga_generator_configuration ast args.ga_target_point
    in
    let generation_callback (inputs : int list) (steps : int) : unit =
      if args.ga_compact_output then
        Printf.printf "[%s]\n%d\n"
          (String.join "," @@ List.map string_of_int inputs)
          steps
      else
        Printf.printf "Input sequence: [%s]\nGenerated in %d steps.\n"
          (String.join ", " @@ List.map string_of_int inputs)
          steps;
      flush stdout;
      results_remaining := Option.map (fun n -> n - 1) !results_remaining;
      if !results_remaining = Some 0 then
        raise GenerationComplete
    in
    try
      let answers, generator_opt =
        Generator.generate_inputs ~generation_callback args.ga_maximum_steps
          generator
      in
      let answer_count = List.length answers in
      if args.ga_compact_output then (
        Printf.printf "%d\n" answer_count;
        if Option.is_none generator_opt then
          print_endline "no"
        else
          print_endline "yes")
      else (
        Printf.printf "%d answer%s generated\n" answer_count
          (if answer_count = 1 then "" else "s");
        if Option.is_none generator_opt then
          print_endline "No further control flows exist."
        else
          print_endline "Further control flows may exist.")
    with GenerationComplete ->
      print_endline "Requested input sequences found; terminating."
  with
  | GenComplete -> ()
  | Odefa_symbolic_interpreter.Interpreter.Invalid_query msg ->
      prerr_endline msg
