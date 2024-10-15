open Core
open Sato_concolic
open Dj_common
module Test_expect = Test_expect_sato

(* TODO: Refactor; there must be a better way of doing this. *)
let errors_to_plain (actual : Sc_result.reported_error) : Test_expect.t =
  let open Sc_result in
  match actual with
  | Jayil_error (err : Jayil_type_errors.t) ->
      let actual_err_loc = Jayil_error_location.show @@ err.err_location in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_odefa (error : Sc_error.Jayil_error.t) :
          Test_expect.error =
        match error with
        | Sc_error.Jayil_error.Error_match err ->
            let actual_aliases =
              List.map ~f:(fun (Ident i, _) -> i) err.err_match_aliases
            in
            let actual_v = Jayil.Pp.Brief.show_clause_body err.err_match_val in
            let a_actual_type, a_expected_type =
              ( Jayil.Pp.Brief.show_type_sig @@ err.err_match_actual,
                Jayil.Pp.Brief.show_type_sig @@ err.err_match_expected )
            in
            Match_error
              {
                m_value = (actual_aliases, actual_v);
                expected_type = a_expected_type;
                actual_type = a_actual_type;
              }
        | Sc_error.Jayil_error.Error_value err ->
            let actual_aliases =
              List.map ~f:(fun (Ident i, _) -> i) err.err_value_aliases
            in
            let actual_v = Jayil.Pp.Brief.show_clause_body err.err_value_val in
            Value_error { v_value = (actual_aliases, actual_v) }
        | _ -> failwith "Expect no other error types!"
      in
      let errs = List.map ~f:transform_one_err_odefa actual_errs in
      {
        found_at_clause = actual_err_loc;
        number_of_errors = err_num;
        error_list = errs;
      }
  | Jay_error (err : Jay_type_errors.t) ->
      let actual_err_loc = Jay_error_location.show @@ err.err_location in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_jay (error : Sc_error.On_error.t) :
          Test_expect.error =
        match error with
        | Sc_error.On_error.Error_match err ->
            let actual_aliases =
              List.map ~f:Jay_error_location.show err.err_match_aliases
            in
            let actual_v = Jay.Jay_ast_pp.show_expr err.err_match_val.body in
            let a_actual_type, a_expected_type =
              ( Jay.Jay_ast_pp.show_jay_type @@ err.err_match_actual,
                Jay.Jay_ast_pp.show_jay_type @@ err.err_match_expected )
            in
            Match_error
              {
                m_value = (actual_aliases, actual_v);
                expected_type = a_expected_type;
                actual_type = a_actual_type;
              }
        | Sc_error.On_error.Error_value err ->
            let actual_aliases =
              List.map ~f:Jay_error_location.show err.err_value_aliases
            in
            let actual_v = Jay.Jay_ast_pp.show_expr err.err_value_val.body in
            Value_error { v_value = (actual_aliases, actual_v) }
        | _ -> failwith "Expect no other error types!"
      in
      let errs = List.map ~f:transform_one_err_jay actual_errs in
      {
        found_at_clause = actual_err_loc;
        number_of_errors = err_num;
        error_list = errs;
      }
  | Bluejay_error err ->
      let actual_err_loc = Bluejay_error_location.show @@ err.err_location in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_tnat (error : Sc_error.Bluejay_error.t) :
          Test_expect.error =
        match error with
        | Sc_error.Bluejay_error.Error_match err ->
            let actual_aliases =
              List.map ~f:Bluejay_error_location.show err.err_match_aliases
            in
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_match_val.body
            in
            let a_actual_type, a_expected_type =
              ( Bluejay.Bluejay_ast_pp.show_bluejay_type @@ err.err_match_actual,
                Bluejay.Bluejay_ast_pp.show_bluejay_type
                @@ err.err_match_expected )
            in
            Match_error
              {
                m_value = (actual_aliases, actual_v);
                expected_type = a_expected_type;
                actual_type = a_actual_type;
              }
        | Sc_error.Bluejay_error.Error_bluejay_type err ->
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_type_variable.body
            in
            let a_actual_type, a_expected_type =
              ( Bluejay.Bluejay_ast_pp.show_expr @@ err.err_type_actual.body,
                Bluejay.Bluejay_ast_pp.show_expr @@ err.err_type_expected.body
              )
            in
            Type_error
              {
                t_var = actual_v;
                t_expected_type = a_expected_type;
                t_actual_type = a_actual_type;
              }
        | Sc_error.Bluejay_error.Error_value err ->
            let actual_aliases =
              List.map ~f:Bluejay_error_location.show err.err_value_aliases
            in
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_value_val.body
            in
            Value_error { v_value = (actual_aliases, actual_v) }
        | _ -> failwith "Expect no other error types!"
      in
      let errs = actual_errs |> List.map ~f:transform_one_err_tnat in
      {
        found_at_clause = actual_err_loc;
        number_of_errors = err_num;
        error_list = errs;
      }

let is_error_expected (actual : Sc_result.reported_error)
    (expected : Test_expect.t) : bool =
  let actual_error = Test_expect.clean_up_t @@ errors_to_plain actual in
  let () = print_endline @@ Test_expect.show actual_error in
  let () = print_endline @@ Test_expect.show expected in
  Test_expect.equal expected actual_error

let test_one_file testname _switch () =
  let config =
    {
      Global_config.default_sato_test_config with
      filename = testname;
      mode = Sato (Dj_common.File_utils.lang_from_file testname);
    }
  in
  let program_full =
    File_utils.read_source_full ~do_wrap:config.is_wrapped
      ~do_instrument:config.is_instrumented config.filename
  in
  let%lwt errors_opt = Sato_concolic.Main.main_lwt ~config program_full in
  let expectation = File_utils.load_expect_s testname in
  let test_result =
    match expectation, errors_opt with
    | None, `No_error _ -> Alcotest.(check bool) "Expect no type errors" true true
    | Some _, `Type_mismatch ->
      Alcotest.(check bool)
        "Type mismatch was found as expected, but no error messages are provided right now" true true
    | Some expected, `Error error ->
        Alcotest.(check bool)
        "Type error matches expected" true
        (is_error_expected error expected)
    | Some _, `No_error _ -> Alcotest.(check bool) "Expect type error!" true false
    | None, _ -> Alcotest.(check bool) "Expect no type errors" true false
in
  Lwt.return @@ test_result

let main test_path =
  let grouped_tests =
    Directory_utils.map_in_groups
      ~f:(fun _ test_name test_path ->
        Alcotest_lwt.test_case test_name `Quick @@ test_one_file test_path)
      test_path
  in
  Lwt_main.run @@ Alcotest_lwt.run "Sato" grouped_tests ;
  ()

let () = main "test/sato"
(* let () = main "test/sato/_playing-ground" *)
