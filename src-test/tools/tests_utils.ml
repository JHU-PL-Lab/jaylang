open Core

open Lang.Ast.Expr
open Lang.Ast_tools.Utils

let make_test_case_from_ast
      (type a) (testname : string) (parse : string -> a statement list) (ast1 : a statement list) =
  let pp_ast1 = String.concat ~sep:"\n\n" (List.map ast1 ~f:(statement_to_string)) in
  let ast2 = parse pp_ast1 in
  let pp_ast2 = String.concat ~sep:"\n\n" (List.map ast2 ~f:(statement_to_string)) in

  Alcotest.test_case testname `Quick (fun () ->
    Alcotest.(check string) "pp_compare" pp_ast1 pp_ast2;
    Alcotest.(check int) "ast_compare" 0 (compare (Lang.Ast.Ident.compare) (pgm_to_module ast1) (pgm_to_module ast2)))
