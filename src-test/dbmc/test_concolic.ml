(* open Core *)
open Dj_common
open Dbmc

let test_one_file testname _args =
  let result = 
    match 
      (* Read expected branch information via sexp from the `<filename>.expect.s` file. *)
      File_utils.load_expect testname Branch_tracker.Status_store.t_of_sexp
    with
    | None -> false (* no expected output, so say it failed *)
    | Some expects ->
      let actual_output = Concolic.eval (Dj_common.File_utils.read_source testname) in
      Branch_tracker.Status_store.compare actual_output expects = 0
  in
  Alcotest.(check bool) "concolic" true result

let () =
  Solver.set_timeout_sec Solver.SuduZ3.ctx (Some (Core.Time_float.Span.of_sec .5));
  let grouped_tests = Lib.group_tests "test/dbmc/concolic" test_one_file in
  Alcotest.run_with_args "concolic" Test_argparse.config grouped_tests
