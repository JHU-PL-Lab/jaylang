open Tests_pp_test_files;;

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
    
