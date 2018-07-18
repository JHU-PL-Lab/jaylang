open Batteries;;
open Jhupllib;;

open Toploop_options;;

let logger = Logger_utils.make_logger "Toploop_main";;
let lazy_logger = Logger_utils.make_lazy_logger "Toploop_main";;

let command_line_parsing () =
  let parser = BatOptParse.OptParser.make ~version:"version 0.3" () in
  add_toploop_option_parsers parser;
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] -> read_parsed_toploop_configuration ()
  | _ -> failwith "Unexpected command-line arguments!"
;;

let () =
  let toploop_configuration = command_line_parsing () in

  print_string "Toploop\n";
  print_string "-------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Parser.parse_expressions IO.stdin
  |> LazyList.iter
    (fun e ->
       print_newline ();
       ignore @@
       Toploop.handle_expression
         ~callbacks:Toploop.stdout_callbacks
         toploop_configuration
         e;
       print_string "\n";
       print_string "Please enter an expression to evaluate followed by \";;\".\n";
       print_string "\n";
       flush stdout
    )
;;
