open Core
open Dj_common
open Dbmc

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
    @@ Dj_common.File_utils.read_source_full ~do_wrap:true ~do_instrument:true testname
  | _ -> failwith "unsupported test extension"
  end
  |> Concolic_driver.test_expr ~quit_on_abort:true
  |> begin function
    | Concolic_driver.Test_result.Timeout
    | Exhausted
    | Exhausted_pruned_tree -> false (* did not find error *)
    | Type_mismatch _
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
      @ make_tests "bjy/scheme-pldi-2015" `Quick
      @ make_tests "bjy/oopsla-24a-additional-tests" `Quick
      @ make_tests "bjy/sato-bjy" `Quick
    ) 
    ~quick_only:true
