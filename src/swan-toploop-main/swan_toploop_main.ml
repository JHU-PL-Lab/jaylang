open Batteries;;
open Jhupllib;;

let logger = Logger_utils.make_logger "Swan_toploop_main";;
let lazy_logger = Logger_utils.make_lazy_logger "Swan_toploop_main";;

let command_line_parsing () =
  let parser = BatOptParse.OptParser.make ~version:"version 0.3" () in
  Core_toploop_options.add_core_toploop_option_parsers parser;
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] -> Core_toploop_options.read_parsed_core_toploop_configuration ()
  | _ -> failwith "Unexpected command-line arguments!"
;;

let () =
  let toploop_configuration = command_line_parsing () in

  print_string "Swan Toploop\n";
  print_string "------------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Swan_parser.parse_expressions IO.stdin
  |> LazyList.iter
    (fun swan_expr ->
       print_newline ();
       ignore @@
       Swan_toploop.handle_expression
         ~callbacks:Swan_toploop.stdout_callbacks
         toploop_configuration
         swan_expr;
       print_string "\n";
       print_string "Please enter an expression to evaluate followed by \";;\".\n";
       print_string "\n";
       flush stdout
    )
;;
