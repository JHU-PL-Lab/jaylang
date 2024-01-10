open Core
open Dj_common
open Dbmc

let test_one_file testname _args =
  let result = 
    match 
      (* Read expected branch information via sexp from the `<filename>.expect.s` file. *)
      File_utils.load_expect testname
      @@ List.t_of_sexp Branch_tracker.Status_store.t_of_sexp
    with
    | None -> false (* no expected output, so say it failed *)
    | Some expect_list ->
      let actual_output = Concolic.eval (Dj_common.File_utils.read_source testname) in
      List.exists expect_list ~f:(fun e -> Branch_tracker.Status_store.compare actual_output e = 0)
  in
  Alcotest.(check bool) "concolic" true result

let () =
  let grouped_tests = Lib.group_tests "test/dbmc/concolic" test_one_file in
  Alcotest.run_with_args "concolic" Test_argparse.config grouped_tests
