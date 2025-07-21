open Core
(* open Concolic *)
open Utils

let make_pp_test_from_filename (testname : Filename.t) : unit Alcotest.test_case = 
  (* TODO:
    Write a routine that willl
    * get all of the text from the provided file (S1)
    * parse S1 into an AST (T1)
    * pretty print T1 into a string (S2)
    * parse S2 into an AST (T2)
    * pretty print T2 into a string (S3)
    * yell if S2 ≠ S3
    This will provide a generally good assessment as to whether the
    pretty printer is working properly.
  *)
  ignore testname; failwith "TODO"

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
    
