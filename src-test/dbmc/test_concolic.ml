open Core
open Dj_common
open Dbmc

(* Expects some specific branch info by the end of the run, thereby checking that concolic evaluator works exactly as expected *)
let test_exact_expected testname _args =
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

(* Just tries to find any abort, and that's all *)
let test_for_abort testname _args =
  let expect_path = Filename.chop_extension testname ^ ".expect.s" in (* existence of this file implies an abort should be found *)
  let is_error_expected = Sys_unix.is_file_exn expect_path in
  let result =
    let output = Concolic.eval ~timeout_sec:10.0 (Dj_common.File_utils.read_source testname) (* allow ten seconds *) in
    if Branch_tracker.Status_store.Without_payload.(compare empty output = 0)
    then not is_error_expected (* concolic timed out, which is good if there is no error expected *)
    else (* concolic finished, so check for existence of an abort *)
      Bool.(=)
        (Branch_tracker.Status_store.Without_payload.contains output Branch_tracker.Status.Without_payload.Found_abort)
        is_error_expected
  in
  Alcotest.(check bool) "bjy concolic" true result

(* Change Lib to allow Quick or Slow flag *)
module From_lib =
  struct
    type 'arg test_one = string -> 'arg -> unit
    let group_tests root speed (test_one : 'arg test_one) =
      Directory_utils.map_in_groups
        ~f:(fun _ test_name test_path ->
          Alcotest.test_case test_name speed @@ test_one test_path)
        root
  end

let () =
  let grouped_tests = From_lib.group_tests "test/dbmc/concolic/exact_expected/" `Quick test_exact_expected in
  let bjy_tests = From_lib.group_tests "test/dbmc/concolic/bjy_tests/" `Slow test_for_abort in
  Alcotest.run_with_args "concolic" Test_argparse.config (bjy_tests @ grouped_tests) ~quick_only:false
