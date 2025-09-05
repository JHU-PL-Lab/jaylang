open Core
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
    Cmdliner.Cmd.eval_value' ~argv:(Array.append [| ""; testname; "-t"; "10.0" |] metadata.flags) Concolic.Driver.Deferred.eval
    |> begin function
      | `Ok status -> Concolic.Common.Status.is_error_found status
      | `Exit i -> raise @@ Invalid_argument (Format.sprintf "Test couldn't evaluate and finished with exit code %d." i)
    end
    |> Bool.(=) is_error_expected
    |> Alcotest.check Alcotest.bool "bjy deferred concolic" true

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
  Alcotest.run "deferred concolic"
  @@ make_tests
    [ "post-oopsla-ill-typed"
    ; "post-oopsla-well-typed"

    (* ; "deep-type-error" *)

    (* ; "interp-ill-typed" *)
    (* ; "interp-well-typed" *)

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

    ; "soft-contract-splayed-well-typed"

    ; "sato-bjy-ill-typed"
    ; "sato-bjy-well-typed"

    ; "type-splayed-recursion-ill-typed"
    ; "type-splayed-recursion-well-typed"

    ; "nondeterministic-types"
    ]
    
