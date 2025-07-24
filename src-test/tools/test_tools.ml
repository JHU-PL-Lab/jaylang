open Core
(* open Concolic *)
open Utils

open Lang.Parse

open Lang.Ast.Expr

open Lang.Ast_tools.Utils

let make_pp_test_from_filename (testname : Filename.t) : unit Alcotest.test_case = 
  let ast1 = parse_program (In_channel.create testname) in
  let pp_ast1 = String.concat ~sep:"\n\n" (List.map ast1 ~f:(statement_to_string)) in
  let ast2 = parse_single_pgm_string pp_ast1 in
  let pp_ast2 = String.concat ~sep:"\n\n" (List.map ast2 ~f:(statement_to_string)) in

  Alcotest.test_case testname `Quick (fun () ->
    Alcotest.(check string) "pp_compare" pp_ast1 pp_ast2;
    Alcotest.(check int) "ast_compare" 0 (compare (Lang.Ast.Ident.compare) (pgm_to_module ast1) (pgm_to_module ast2)))

  (* TODO:
    Write a routine that will
    * get all of the text from the provided file (S1)
    * parse S1 into an AST (T1)
    * pretty print T1 into a string (S2)
    * parse S2 into an AST (T2)
    * pretty print T2 into a string (S3)
    * yell if S2 ≠ S3
    This will provide a generally good assessment as to whether the
    pretty printer is working properly.
  *)

let root_dir = "test/bjy/"

let make_pp_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      >>| make_pp_test_from_filename
    )

let () =
  Alcotest.run "concolic"
  @@ make_pp_tests
    [ "post-oopsla-ill-typed"
    ; "post-oopsla-well-typed"

    ; "deep-type-error"

    ; "interp-ill-typed"
    ; "interp-well-typed"

    ; "edge-cases-ill-typed"
    ; "edge-cases-well-typed"

    ; "deterministic-functions-well-typed"
    ; "deterministic-functions-ill-typed"

    ; "functors-ill-typed"
    ; "functors-well-typed"

    ; "oopsla-24-tests-ill-typed"
    ; "oopsla-24-tests-well-typed"

    ; "oopsla-24-benchmarks-ill-typed"
    ; "oopsla-24-benchmarks-well-typed"

    ; "soft-contract-ill-typed"
    ; "soft-contract-well-typed"

    ; "sato-bjy-ill-typed"
    ; "sato-bjy-well-typed"

    ; "type-splayed-recursion-ill-typed"
    ; "type-splayed-recursion-well-typed"
    ]
    
