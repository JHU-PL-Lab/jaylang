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
  let pp_ast1 = Program.to_string ast1 in
  print_endline "\n\nS-exp 1:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast1);
  print_endline "\n\nPass 1:";
  print_endline pp_ast1;
  let ast2 = parse pp_ast1 in
  let pp_ast2 = Program.to_string ast2 in
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
    do_test Bluejay.parse_single_pgm_string text
  | Some SomeLanguage DesugaredLanguage ->
    do_test Desugared.parse_single_pgm_string text
  | Some SomeLanguage EmbeddedLanguage ->
    do_test Embedded.parse_single_pgm_string text
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
  let txt = Program.to_string ast in
  print_endline "\n\nPretty-printed:";
  print_endline txt;
  let ast' = Bluejay.parse_single_pgm_string txt in
  print_endline "\n\nS-exp 1:";
  print_endline (Core.Sexp.to_string_hum @@ prog_to_sexp ast');
  print_endline (Printf.sprintf "\n\nMatch? %d" (Expr.compare_program Ident.compare ast ast'));
;;
ignore main_explicit_ast;;

let main_print_holes_file () =
  let filename = (Sys.get_argv ()).(1) in
  let text = In_channel.input_all (In_channel.create filename) in
  let open Lang.Shrink in
  let do_print_holes (type lang) (prog : lang Expr.statement list) : unit =
    enumerate_deep_points (NodeTypeList NodeTypeStatement) prog
    |> Sequence.to_list
    |> List.iteri ~f:(
      fun n point ->
        let (SomeTransformPoint (type hole)
               (TransformPoint
                  { hole_type;
                    encloser_type = _;
                    hole_node = _;
                    context_fn
                  }
                : (lang, lang Expr.statement list, hole) transform_point)) =
          point
        in
        let context_arg : hole =
          let rec mk_dummy : type tyhole. (lang, tyhole) node_type -> tyhole =
            fun ty ->
              match ty with
              | NodeTypeExpr -> Expr.EVar (Ident.Ident "HOLE")
              | NodeTypePattern -> Pattern.PVariable (Ident.Ident "HOLE")
              | NodeTypeStatement ->
                Expr.SUntyped { var = (Ident.Ident "HOLE");
                                defn = Expr.EVar (Ident.Ident "HOLE") }
              | NodeTypeList ty' -> [mk_dummy ty']
          in
          mk_dummy hole_type
        in
        let dummy_ast = context_fn context_arg in
        print_endline (Printf.sprintf "-------------------------- #%d" n);
        print_endline (Program.to_string dummy_ast);
    )
  in
  match extension_to_language (Stdlib.Filename.extension filename) with
  | Some SomeLanguage BluejayLanguage ->
    do_print_holes @@ Bluejay.parse_single_pgm_string text
  | Some SomeLanguage DesugaredLanguage ->
    do_print_holes @@ Desugared.parse_single_pgm_string text
  | Some SomeLanguage EmbeddedLanguage ->
    do_print_holes @@ Embedded.parse_single_pgm_string text
  | None ->
    print_endline ("Unrecognized extension: " ^ Stdlib.Filename.extension filename)
;;

(* main_test_file ();; *)
(* main_explicit_ast ();; *)
main_print_holes_file ();;