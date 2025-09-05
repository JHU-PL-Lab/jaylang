open Core

open Lang.Ast
open Lang.Ast.Expr
open Lang.Ast_tools.Utils
open Lang.Parser

let () = Stdlib.ignore stmt_to_expr;;

let prog_to_sexp (type lang) (prog : lang statement list) : Core.Sexp.t =
  Utils.Sexp_utils.mk_list Sexp.statement_to_sexp prog

let do_test
    (parse : string -> 'a statement list) 
    (statement_to_string : 'a statement -> string)
    (text : string)
  : unit =
  let parse s =
    try
      parse s
    with | exn ->
      print_endline "Parse failure";
      raise exn
  in
  print_endline "Orginal:";
  print_endline text;
  let ast1 = parse text in
  let pp_ast1 = String.concat ~sep:"\n\n" (List.map ast1 ~f:(statement_to_string)) in
  print_endline "\n\nS-exp 1:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast1);
  print_endline "\n\nPass 1:";
  print_endline pp_ast1;
  let ast2 = parse pp_ast1 in
  let pp_ast2 = String.concat ~sep:"\n\n" (List.map ast2 ~f:(statement_to_string)) in
  print_endline "\n\nS-exp 2:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast2);
  print_endline "\n\nPass 2:";
  print_endline pp_ast2;
  print_endline "\n\nAST 1 <=>? AST 2:";
  print_endline (string_of_int (Lang.Ast.Expr.compare Lang.Ast.Ident.compare (pgm_to_module ast1) (pgm_to_module ast2)));
;;
ignore do_test;;

let main_test_file () =
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
;;
ignore main_test_file;;

let main_explicit_ast () =
  let vtl s = VariantTypeLabel.VariantTypeLabel (Ident.Ident s) in
  let ast = 
    [SUntyped {
        var = Ident.Ident "x";
        defn =
          ETypeFun {
            domain = ETypeVariant [(vtl "B", ETypeInt)];
            codomain = ETypeVariant [
                (vtl "Q", ETypeVariant [
                  (vtl "X", ETypeInt);
                ]);
                (vtl "R", ETypeInt);
              ];
            det = false;
            dep = `No;
          };
      }]
  in
  print_endline "\n\nS-exp original:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast);
  let txt =
    String.concat ~sep:"\n\n" @@ List.map ~f:Expr.statement_to_string ast
  in
  print_endline "\n\nPretty-printed:";
  print_endline txt;
  let ast' = Bluejay.parse_single_pgm_string txt in
  print_endline "\n\nS-exp 1:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast');
  print_endline (Printf.sprintf "\n\nMatch? %d" (Expr.compare_program Ident.compare ast ast'));
;;
ignore main_explicit_ast;;

main_test_file ();;
(* main_explicit_ast ();; *)