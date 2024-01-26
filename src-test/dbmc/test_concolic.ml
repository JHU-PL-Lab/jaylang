open Core
open Dj_common
open Dbmc

let test_one_file testname _args =
  let result = 
    match 
      (* Read expected branch information via sexp from the `<filename>.expect.s` file. *)
      File_utils.load_expect testname
      @@ List.t_of_sexp Branch_tracker.Status_store.Without_payload.t_of_sexp
    with
    | None -> false (* no expected output, so say it failed *)
    | Some expect_list ->
      let actual_output = Concolic.eval ~quit_on_first_abort:false (Dj_common.File_utils.read_source testname) in
      List.exists expect_list ~f:(fun e -> Branch_tracker.Status_store.Without_payload.compare actual_output e = 0)
  in
  Alcotest.(check bool) "concolic" true result

let test_bjy_conv testname _args =
  let expect_path = Filename.chop_extension testname ^ ".expect.s" in
  let is_error_expected = Sys_unix.is_file_exn expect_path in
  let result =
    match
      Concolic.eval_timeout (Dj_common.File_utils.read_source testname) 10.0 (* allow ten seconds *)
    with
    | None -> not is_error_expected (* concolic timed out, which is good if there is no error expected *)
    | Some output -> (* concolic finished, so check for existence of an abort *)
      Bool.(=)
        (Branch_tracker.Status_store.Without_payload.contains output Branch_tracker.Status.Without_payload.Found_abort)
        is_error_expected
  in
  Alcotest.(check bool) "bjy concolic" true result

let () =
  let grouped_tests = Lib.group_tests "test/dbmc/concolic/" test_one_file in
  let bjy_tests = Lib.group_tests "test/dbmc/concolic/_bjy_tests/" test_bjy_conv in
  Alcotest.run_with_args "concolic" Test_argparse.config (bjy_tests @ grouped_tests)
