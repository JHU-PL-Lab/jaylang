open Core
open Lang
open Utils
open Interpreter

(*
  This just runs some acceptance tests on the interpreter. We
  * Check that the program can be run in all three languages
  * A well-typed program never hits an error
  * An ill-typed program may or may not hit an error
*)
let testcases_of_filename (testname : Filename.t) : unit Alcotest.test_case list = 
  let metadata = Metadata.of_bjy_file testname in
  let is_error_expected = 
    match metadata.typing with
    | Ill_typed -> true
    | Well_typed | Exhausted -> false
  in
  let check pgm =
    let is_error = Interp.V.is_error (Interp.eval_pgm pgm) in
    if is_error_expected 
    then (* any result okay because this run may or may not hit error *)
      Alcotest.check Alcotest.pass "interp" () ()
    else (* definitely should not have hit the error *)
      Alcotest.check Alcotest.bool "interp" true (not is_error)
  in
  let make convert =
    Alcotest.test_case testname `Quick
    @@ fun () ->
    let bjy =
      Parser.Bluejay.parse_single_pgm_string @@ In_channel.read_all testname in
    check (convert bjy)
  in
  (* Any program using `abstract` cannot be executed in desugared mode because it's meant purely as an intermediate step. Just never run the desugared interpreter. *)
  [ make Translate.Convert.bjy_to_erased ; make Fn.id (*; make (Translate.Convert.bjy_to_des ~do_type_splay:No)*) ; make (Translate.Convert.bjy_to_emb ~do_wrap:true ~do_type_splay:No) ]

let root_dir = "test/bjy/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
  ( dirname
  , [ root_dir ^ dirname ]
    |> File_utils.get_all_bjy_files
    >>= testcases_of_filename
  )

let () =
  Alcotest.run "interp"
  @@ make_tests
    [ "oopsla-26-ill-typed"
    ; "oopsla-26-well-typed"

    ; "ocaml-functors-ill-typed"
    ; "ocaml-functors-well-typed"

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
    ]

