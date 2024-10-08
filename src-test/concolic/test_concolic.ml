open Core
open Dj_common
open Concolic

type 'arg test_one = string -> 'arg -> unit

let group_tests root speed (test_one : 'arg test_one) =
  Directory_utils.map_in_groups
    ~f:(fun _ test_name test_path ->
      Alcotest.test_case test_name speed @@ test_one test_path)
    root

let test_for_abort is_error_expected testname _args = 
  let _, extension = Filename.split_extension testname in
  begin
  match extension with
  | Some "jil" -> Dj_common.File_utils.read_source testname
  | Some "bjy" ->
    Convert.jil_ast_of_convert
    @@ Dj_common.File_utils.read_source_full ~do_wrap:true ~do_instrument:false testname
  | _ -> failwith "unsupported test extension"
  end
  |> Driver.test_expr ~global_timeout_sec:30.0
  |> Driver.Test_result.is_error_found
  |> Bool.(=) is_error_expected
  |> Alcotest.(check bool) "bjy concolic" true

let dir = "test/concolic/"

let make_tests e s t = group_tests (dir ^ s) t (test_for_abort e)

let make_tests_well_typed s = make_tests false s `Slow
let make_tests_ill_typed s = make_tests true s `Quick

let () =
  Alcotest.run_with_args 
    "concolic" 
    Test_argparse.config 
    (
      []
      (* @ make_tests_ill_typed "bjy/deep-type-error" *)

      @ make_tests_ill_typed "bjy/buggy-ill-typed"
      @ make_tests_well_typed "bjy/buggy-well-typed"

      @ make_tests_ill_typed "bjy/oopsla-24-tests-ill-typed"
      @ make_tests_well_typed "bjy/oopsla-24-tests-well-typed"

      @ make_tests_ill_typed "bjy/oopsla-24-benchmarks-ill-typed"
      @ make_tests_well_typed "bjy/oopsla-24-benchmarks-well-typed"

      @ make_tests_ill_typed "bjy/scheme-pldi-2015-ill-typed"
      @ make_tests_well_typed "bjy/scheme-pldi-2015-well-typed"

      @ make_tests_ill_typed "bjy/sato-bjy-ill-typed"
      @ make_tests_well_typed "bjy/sato-bjy-well-typed"
    ) 
    ~quick_only:true
