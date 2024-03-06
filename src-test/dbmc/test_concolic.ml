open Core
open Dj_common
open Dbmc

[@@@ocaml.warning "-32"]
(* Expects some specific branch info by the end of the run, thereby checking that concolic evaluator works exactly as expected *)
let test_exact_expected testname _args =
  let result = 
    match 
      (* Read expected branch information via sexp from the `<filename>.expect.s` file. *)
      File_utils.load_expect testname
      @@ List.t_of_sexp Branch_info.t_of_sexp
    with
    | None -> false (* no expected output, so say it failed *)
    | Some expect_list ->
      let actual_output = Concolic.eval ~quit_on_abort:false (Dj_common.File_utils.read_source testname) in
      List.exists expect_list ~f:(fun e -> Branch_info.compare actual_output e = 0)
  in
  Alcotest.(check bool) "concolic" true result
[@@@ocaml.warning "+32"]

(* Just tries to find any abort, and that's all *)
let test_for_abort testname _args =
  let expect_path = Filename.chop_extension testname ^ ".expect.s" in (* existence of this file implies an abort should be found *)
  let is_error_expected = Sys_unix.is_file_exn expect_path in
  Concolic_driver.test ~global_timeout_sec:10.0 ~quit_on_abort:true testname
  |> begin function
    | `Timeout
    | `Exhausted
    | `Exhausted_pruned_tree -> false (* did not find error *)
    | `Found_abort -> true (* found error *)
  end
  |> Bool.(=) is_error_expected
  |> Alcotest.(check bool) "bjy concolic" true

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
  (* let grouped_tests = From_lib.group_tests "test/dbmc/concolic/exact_expected/" `Slow test_exact_expected in *) (* TODO: fix expected results *)
  let grouped_tests = [] in
  let _bjy_tests = From_lib.group_tests "test/dbmc/concolic/_bjy_tests/" `Slow test_for_abort in
  let bjy_tests = From_lib.group_tests "test/dbmc/concolic/bjy_tests/" `Quick test_for_abort in
  Alcotest.run_with_args "concolic" Test_argparse.config (bjy_tests @ _bjy_tests @ grouped_tests) ~quick_only:false
