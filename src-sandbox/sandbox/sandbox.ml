open Core

open Lang.Ast
open Lang.Ast.Expr
open Lang.Ast_tools.Utils
open Lang.Parser

let do_test
    (parse : string -> 'a statement list) 
    (statement_to_string : 'a statement -> string)
    (text : string)
  : unit =
  let ast1 = parse text in
  let pp_ast1 = String.concat ~sep:"\n\n" (List.map ast1 ~f:(statement_to_string)) in
  let ast2 = parse pp_ast1 in
  let pp_ast2 = String.concat ~sep:"\n\n" (List.map ast2 ~f:(statement_to_string)) in
  print_endline "Orginal:";
  print_endline text;
  print_endline "\n\nPass 1:";
  print_endline pp_ast1;
  print_endline "\n\nPass 2:";
  print_endline pp_ast2;
  print_endline "\n\nAST 1 <=>? AST 2:";
  print_endline (string_of_int (Lang.Ast.Expr.compare Lang.Ast.Ident.compare (pgm_to_module ast1) (pgm_to_module ast2)));  
;;

let () =
  let filename = (Sys.get_argv ()).(1) in
  let text = In_channel.input_all (In_channel.create filename) in
  match extension_to_language (Stdlib.Filename.extension filename) with
  | Some SomeLanguage BluejayLanguage ->
    do_test Bluejay.parse_single_pgm_string statement_to_string text
  | Some SomeLanguage DesugaredLanguage ->
    do_test Desugared.parse_single_pgm_string statement_to_string text
  | Some SomeLanguage EmbeddedLanguage ->
    do_test Embedded.parse_single_pgm_string statement_to_string text
  | None ->
    print_endline ("Unrecognized extension: " ^ Stdlib.Filename.extension filename)
