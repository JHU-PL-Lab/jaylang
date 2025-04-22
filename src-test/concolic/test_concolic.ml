open Core
open Concolic
open Utils

let testcase_of_filename (testname : Filename.t) : unit Alcotest.test_case = 
  let metadata = Metadata.of_bjy_file testname in
  let is_error_expected = 
    match metadata.typing with
    | Ill_typed -> true
    | Well_typed -> false
  in
  let speed_level =
    match metadata.speed with
    | Slow -> `Slow
    | Fast -> `Quick
  in
  Alcotest.test_case testname speed_level
  @@ fun () ->
    testname
    |> Driver.test ~global_timeout_sec:30.0 ~do_wrap:true ~in_parallel:false (* parallel computation off by default *)
    |> Status.is_error_found
    |> Bool.(=) is_error_expected
    |> Alcotest.check Alcotest.bool "bjy concolic" true

let root_dir = "test/bjy/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      >>| testcase_of_filename
    )

let () =
  Alcotest.run "concolic"
  @@ make_tests
    [ "post-oopsla-ill-typed"
    ; "post-oopsla-well-typed"

    (* ; "deep-type-error" *)

    (* ; "interp-ill-typed" *)
    (* ; "interp-well-typed" *)

    ; "edge-cases-ill-typed"
    ; "edge-cases-well-typed"

    ; "oopsla-24-tests-ill-typed"
    ; "oopsla-24-tests-well-typed"

    ; "oopsla-24-benchmarks-ill-typed"
    ; "oopsla-24-benchmarks-well-typed"

    ; "scheme-pldi-2015-ill-typed"
    ; "scheme-pldi-2015-well-typed"

    ; "sato-bjy-ill-typed"
    ; "sato-bjy-well-typed"

    ; "counter-ill-typed"
    ]
    
