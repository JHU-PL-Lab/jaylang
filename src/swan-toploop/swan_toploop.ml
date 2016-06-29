open Batteries;;

open A_translator;;
open Core_ast_pp;;
open Core_ast_wellformedness;;
open Core_interpreter;;
open Swan_translator;;
open Toploop_options;;

let lazy_logger = Logger_utils.make_lazy_logger "Swan_toploop";;

let toploop_operate () (e:Swan_ast.expr)=
  print_string "\n";
  begin
    try
      lazy_logger `trace (fun () ->
          Printf.sprintf "Parsed expression: %s" (Swan_ast.show_expr e));
      let (e'', _) = swan_to_nested_translation e in
      let (e',_) = a_translate_nested_expr e'' in
      check_wellformed_expr e';
      let v,env = eval e' in
      print_string (show_var v ^ " where "  ^ show_env env ^ "\n");
    with
    | Illformedness_found(ills) ->
      print_string "Provided expression is ill-formed:\n";
      List.iter
        (fun ill ->
           print_string @@ "   " ^ show_illformedness ill ^ "\n")
        ills
  end;
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout
;;

let command_line_parsing () =
  let parser = BatOptParse.OptParser.make ~version:"version 0.3" () in
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] -> ()
  | _ -> failwith "Unexpected command-line arguments."
;;

let () =
  let toploop_options = command_line_parsing () in

  print_string "Toy Swan Toploop\n";
  print_string "------------------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Swan_parser.parse_expressions IO.stdin
  |> LazyList.iter (toploop_operate toploop_options)
;;
