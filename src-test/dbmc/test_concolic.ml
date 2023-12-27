(* open Core *)
open Dj_common
open Dbmc

let test_one testname _args =
  let reset_limit = 20 in
  Concolic_driver.test_program_concolic testname reset_limit ;
  Alcotest.(check bool) "concolic" true true

let () =
  let grouped_tests = Lib.group_tests "test/dbmc/concolic" test_one in
  Alcotest.run_with_args "concolic" Test_argparse.config grouped_tests
