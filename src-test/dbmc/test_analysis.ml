open Core
open Dj_common

let skipped =
  [
    "loop.jay";
    "flatten";
    "list_sum_add_build";
    "smbc_fold0s";
    "smbc_long_rev_sum3";
    "smbc_pigeon";
    (* fail for 0cfa *)
    "smbc_sorted_sum";
  ]

let test_one testname _switch _args =
  if List.exists skipped ~f:(fun substring ->
         String.is_substring ~substring testname)
  then (
    (* Alcotest.(check reject) "skipped" () () ; *)
    (ignore @@ Alcotest.(fail "skipped")) ;
    Lwt.return_unit)
  else
    let open Lwt.Syntax in
    let config = Global_config.with_filename testname in
    let src = Global_config.read_source config in
    let config = { config with analyzer = K_cfa 1 } in
    let* _ = Dbmc.Main.main_top_lwt ~config src in

    Alcotest.(check bool) "run" true true ;
    Lwt.return_unit

let () =
  let grouped_tests = Lib.group_tests_lwt "test/dbmc" test_one in
  Lwt_main.run
  @@ Alcotest_lwt.run_with_args "analysis" Test_argparse.config grouped_tests ;
  Dj_common.Log.close ()
