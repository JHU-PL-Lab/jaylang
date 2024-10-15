open Core
open Dj_common

let test_one testname _switch _args =
  let open Lwt.Syntax in
  let config = Global_config.with_filename testname in
  let src = Global_config.read_source config in
  let* _ = Dbmc.Main.main_top_lwt ~config src in

  Alcotest.(check bool) "dummy" true true ;
  Lwt.return_unit

let () =
  let grouped_tests = Lib.group_tests_lwt "test/dbmc" test_one in
  Lwt_main.run
  @@ Alcotest_lwt.run_with_args "Dummy" Test_argparse.config grouped_tests ;
  Dj_common.Log.close ()
