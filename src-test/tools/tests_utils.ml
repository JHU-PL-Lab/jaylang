open Core

open Lang.Ast.Expr
open Lang.Ast_tools.Utils

type 'a test_result =
  | Success
  | Pp_parse_failure of { original: 'a statement list;
                          pp_code : string;
                          exn : exn; }
  | Ast_mismatch of { code : string;
                      ast1 : 'a statement list;
                      ast2 : 'a statement list; }

let is_success (result : 'a test_result) : bool =
  match result with
  | Success -> true
  | _ -> false

let do_test
    (type a)
    (parse : string -> a statement list)
    (ast : a statement list)
  : a test_result =
  let ast1 = ast in
  let ast_text_1 = Lang.Ast.Program.to_string ast1 in
  try
    let ast2 = parse ast_text_1 in
    if compare
        Lang.Ast.Ident.compare
        (pgm_to_module ast1)
        (pgm_to_module ast2) <> 0 then
      Ast_mismatch { code = ast_text_1; ast1; ast2 }
    else
      Success
  with
  | exn ->
    Pp_parse_failure { original = ast; pp_code = ast_text_1; exn }

let prog_to_sexp (type lang) (prog : lang statement list) : Core.Sexp.t =
  Utils.Sexp_utils.mk_list Sexp.statement_to_sexp prog

let prog_to_sexp_text (type lang) (prog : lang statement list) : string =
  Core.Sexp.to_string_hum @@ prog_to_sexp prog

let react_to_result (result : 'a test_result) =
  match result with
  | Success -> ()
  | Pp_parse_failure { original; pp_code; exn } ->
    print_endline
      (Printf.sprintf "Failed to parse output of pretty printer:\n%s" pp_code);
    print_endline
      (Printf.sprintf "Original AST was:\n%s" (prog_to_sexp_text original));
    raise exn
  | Ast_mismatch { code; ast1; ast2 } ->
    Alcotest.fail (
      Printf.sprintf
        ("ASTs differed!  Source:\n\n%s" ^^
         "\n\nFirst raw AST:\n\n%s\n\nSecond raw AST:\n\n%s\n\n")
        code
        (prog_to_sexp_text ast1)
        (prog_to_sexp_text ast2)
    )

let make_test_case_from_ast
    (type a)
    ?(shrink:bool=false)
    (testname : string)
    (parse : string -> a statement list)
    (ast_gen : unit -> a statement list) =
  Alcotest.test_case testname `Quick (
    fun () ->
      let ast : a Lang.Ast.Expr.statement list = ast_gen () in
      let result = do_test parse ast in
      if is_success result then react_to_result result else
      if shrink then
        let shrunken_ast : a Lang.Ast.Expr.statement list =
          Lang.Shrink.shrink
            (fun (new_ast : a Lang.Ast.Expr.statement list) ->
               (not @@ List.is_empty new_ast) &&
               (not @@ is_success @@ do_test parse new_ast))
            (Lang.Shrink.NodeTypeList Lang.Shrink.NodeTypeStatement)
            ast
        in
        react_to_result @@ do_test parse shrunken_ast
      else
        react_to_result result
  )