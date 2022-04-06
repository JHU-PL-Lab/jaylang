open Batteries
open Jhupllib
open Odefa_toploop
open Toploop_options
open Langdk

let logger = Logger_utils.make_logger "Toploop_main"
let lazy_logger = Logger_utils.make_lazy_logger "Toploop_main"

let command_line_parsing () =
  let parser = BatOptParse.OptParser.make ~version:"version 0.3.1" () in
  add_toploop_option_parsers parser ;
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] -> read_parsed_toploop_configuration ()
  | _ -> failwith "Unexpected command-line arguments!"

let () =
  let toploop_configuration = command_line_parsing () in

  print_string "Toploop\n" ;
  print_string "-------\n" ;
  print_string "\n" ;
  print_string "Please enter an expression to evaluate followed by \";;\".\n" ;
  print_string "\n" ;
  flush stdout ;

  let module Echo_E = (val (module struct
                             type e = string

                             let eval e0 =
                               let es =
                                 Odefa_parser.Parse.parse_expressions_str e0
                               in
                               LazyList.iter
                                 (fun e ->
                                   ignore
                                   @@ Toploop.handle_expression
                                        ~callbacks:Toploop.stdout_callbacks
                                        toploop_configuration e)
                                 es ;
                               e0

                             let of_string e = e
                             let to_string e = e
                           end) : Repl.E_sig)
  in
  let module Echo_repl = Repl.Make (Echo_E) in
  Echo_repl.run ()