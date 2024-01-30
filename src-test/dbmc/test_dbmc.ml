open Core
open Dj_common
open Dbmc

let test_one_file testname _switch test_config =
  let open Lwt.Syntax in
  (* expects are the list of expect from expect file,
     or a default expect if no expect file *)
  let expects = File_utils.load_expect_with_default testname in
  let config = Test_argparse.lift_to_global_config test_config testname in
  let src = Global_config.read_source config in
  Dj_common.Log.init config ;

  Lwt_list.iter_s
    (fun (expect : Test_expect.one_case) ->
      (* an expect that can be
         1. with a default ok-to-any input to reach the default `target`
         2. with a special input spec saying the target is unreachable
         3. with a specified input expectation to reach a specified points

         How does it distinguish with an test case just reach the target point
         with no input required? Is it also an expect with no input.
      *)
      let config = Global_config.with_expect expect config in
      let* { is_timeout; inputss; state; _ } = Main.main_top_lwt ~config src in

      prerr_endline ("len of inputss " ^ string_of_int (List.length inputss)) ;
      prerr_endline ("spec " ^ Input_spec.show expect.inputs) ;

      if is_timeout then Alcotest.(check bool) "timeout" false is_timeout ;

      (* `List.is_empty inputss` means unreachable. Few cases should be unreachable *)
      let reachable = not (List.is_empty inputss) in

      (if Input_spec.is_unreachable expect.inputs
       then Alcotest.(check bool) "should be unreachable" false reachable
       else if Input_spec.is_any_input expect.inputs
       then Alcotest.(check bool) "should be reachable" true reachable
       else
         let reachable_with_expected_input =
           Main.check_expected_input ~config ~state expect.inputs
         in
         Alcotest.(check bool)
           "expected input" true reachable_with_expected_input) ;
      Lwt.return_unit)
    expects

let () =
  let top_config = Test_argparse.parse_test_commandline () in
  let grouped_tests = Lib.group_tests_lwt top_config.test_path test_one_file in
  Lwt_main.run
  @@ Alcotest_lwt.run_with_args "DBMC" Test_argparse.config grouped_tests ;
  Dj_common.Log.close ()
