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

let _ = Filename.split_extension

(* Just tries to find any abort, and that's all *)
let test_for_abort testname _args =
  let filename, extension = Filename.split_extension testname in
  let expect_path = filename ^ ".expect.s" in (* existence of this file implies an abort should be found *)
  let is_error_expected = Sys_unix.is_file_exn expect_path in
  begin
  match extension with
  | Some "jil" -> Dj_common.File_utils.read_source testname
  | Some "bjy" ->
    Convert.jil_ast_of_convert
    @@ Dj_common.File_utils.read_source_full ~do_instrument:true testname
  | _ -> failwith "unsupported test extension"
  end
  |> Concolic.test ~global_timeout_sec:10.0 ~quit_on_abort:true
  |> begin function
    | Concolic.Test_result.Timeout
    | Exhausted
    | Exhausted_pruned_tree -> false (* did not find error *)
    | Found_abort _ -> true (* found error *)
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
  let dir = "test/dbmc/concolic/" in
  let make_tests s t = From_lib.group_tests (dir ^ s) t test_for_abort in
  Alcotest.run_with_args 
    "concolic" 
    Test_argparse.config 
    (
      []
      (* @ make_tests "_bjy_tests" `Slow not all expect files exist yet, so these tests are not supposed to pass *)
      @ make_tests "bjy_tests" `Quick
      @ make_tests "racket_tests" `Quick
      @ make_tests "racket_tests_well_typed" `Slow
    ) 
    ~quick_only:false
