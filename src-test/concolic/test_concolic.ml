open Core
open Concolic
open Utils

type 'arg test_one = string -> 'arg -> unit

let group_tests root speed (test_one : 'arg test_one) =
  File_utils.Dirs.map_in_groups
    ~f:(fun _ test_name test_path ->
      Alcotest.test_case test_name speed @@ test_one test_path)
    root

let test_for_abort is_error_expected testname _args = 
  testname
  |> Driver.test ~global_timeout_sec:30.0 ~do_wrap:true
  |> Status.is_error_found
  |> Bool.(=) is_error_expected
  |> Alcotest.(check bool) "bjy concolic" true

let dir = "test/bjy/"

let make_tests e s t = group_tests (dir ^ s) t (test_for_abort e)

let[@ocaml.warning "-32"] make_tests_well_typed s = make_tests false s `Slow
let make_tests_ill_typed s = make_tests true s `Quick

let () =
  Alcotest.run_with_args 
    "concolic" 
    Cmdliner.Term.main_name
    (
      []
      (* @ make_tests_ill_typed "deep-type-error" *)

      (* @ make_tests_ill_typed "interp-ill-typed" *)
      (* @ make_tests_well_typed "interp-well-typed" *)

      (* @ make_tests_ill_typed "post-oopsla-ill-typed" *)
      @ make_tests_well_typed "post-oopsla-well-typed"

      @ make_tests_ill_typed "edge-cases-ill-typed"
      @ make_tests_well_typed "edge-cases-well-typed"

      @ make_tests_ill_typed "oopsla-24-tests-ill-typed"
      @ make_tests_well_typed "oopsla-24-tests-well-typed"

      @ make_tests_ill_typed "oopsla-24-benchmarks-ill-typed"
      @ make_tests_well_typed "oopsla-24-benchmarks-well-typed"

      @ make_tests_ill_typed "scheme-pldi-2015-ill-typed"
      @ make_tests_well_typed "scheme-pldi-2015-well-typed"

      @ make_tests_ill_typed "sato-bjy-ill-typed"
      @ make_tests_well_typed "sato-bjy-well-typed"
    ) 
    ~quick_only:true
