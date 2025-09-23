open Tests_pp_test_files
open Tests_randomly_generated_asts

let () =
  let pp_tests =
    make_pp_tests
      [ "oopsla-26-ill-typed"
      ; "oopsla-26-well-typed"

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
  in
  let rand_tests =
    make_rand_tests 100
  in
  Lang.Value.default_to_string_closure_depth := Int.max_int;
  Alcotest.run "concolic"
    (pp_tests @ rand_tests)
