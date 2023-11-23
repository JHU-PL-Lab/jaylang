open Core
open Dj_common
open Dbmc
open Test_argparse

let test_one_file testname _switch test_config =
  let open Lwt.Syntax in
  let expects = File_utils.load_expect_d testname in
  let config = Test_argparse.lift_to_global_config test_config testname in
  let src = Global_config.read_source config in
  Dj_common.Log.init config ;

  Lwt_list.iter_s
    (fun (expect : Test_expect.one_case) ->
      let config = Global_config.with_expect expect config in
      let* { is_timeout; inputss; state; _ } = Main.main_top_lwt ~config src in

      prerr_endline (string_of_bool is_timeout) ;
      if is_timeout then Alcotest.(check bool) "timeout" false is_timeout ;

      (if (not (List.is_empty inputss))
          && not (Input_spec.is_no_spec expect.inputs)
       then
         let reachable =
           Main.check_expected_input ~config ~state expect.inputs
         in
         Alcotest.(check bool) "expected input" true reachable) ;

      Lwt.return_unit)
    expects

let () =
  let top_config = Test_argparse.parse_test_commandline () in
  let grouped_tests =
    Directory_utils.map_in_groups
      ~f:(fun _ test_name test_path ->
        Alcotest_lwt.test_case test_name `Quick @@ test_one_file test_path)
      top_config.test_path
  in
  Lwt_main.run
  @@ Alcotest_lwt.run_with_args "DBMC" Test_argparse.config grouped_tests ;
  Dj_common.Log.close ()
