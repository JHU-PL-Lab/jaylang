open Core
open Sato

(* treat the path as the group name and filename as the test name *)
let group_all_files dir =
  let rec loop dir =
    let acc_f, acc_p =
      Sys_unix.fold_dir ~init:([], [])
        ~f:(fun (acc_f, acc_p) path ->
          match String.get path 0 with
          | '.' (* including "." ".." *) | '_' -> (acc_f, acc_p)
          | _ -> (
              let fullpath = Filename.concat dir path in
              match Sys_unix.is_directory fullpath with
              | `Yes -> (acc_f, loop fullpath @ acc_p)
              | `No when Dj_common.File_utils.check_upto_bluejay fullpath ->
                  (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

(* TODO: Refactor; there must be a better way of doing this. *)
let errors_to_plain (actual : Sato_result.reported_error) : Test_expect.t =
  let open Sato_result in
  match actual with
  | Jayil_error (err : Jayil_type_errors.t) ->
      let actual_err_loc = Jayil_error_location.show @@ err.err_location in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_odefa (error : Sato_error.Jayil_error.t) :
          Test_expect.error =
        match error with
        | Sato_error.Jayil_error.Error_match err ->
            let actual_aliases =
              List.map ~f:(fun (Ident i, _) -> i) err.err_match_aliases
            in
            let actual_v =
              Jayil.Ast_pp_brief.show_clause_body err.err_match_val
            in
            let a_actual_type, a_expected_type =
              ( Jayil.Ast_pp.show_type_sig @@ err.err_match_actual,
                Jayil.Ast_pp.show_type_sig @@ err.err_match_expected )
            in
            Match_error
              {
                m_value = (actual_aliases, actual_v);
                expected_type = a_expected_type;
                actual_type = a_actual_type;
              }
        | Sato_error.Jayil_error.Error_value err ->
            let actual_aliases =
              List.map ~f:(fun (Ident i, _) -> i) err.err_value_aliases
            in
            let actual_v =
              Jayil.Ast_pp_brief.show_clause_body err.err_value_val
            in
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
      (* TODO: Fix - very hacky *)
      let actual_err_loc =
        Jay_error_location.show @@ err.err_location
        |> String.substr_replace_all ~pattern:"\n" ~with_:" "
      in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_jay (error : Sato_error.On_error.t) :
          Test_expect.error =
        match error with
        | Sato_error.On_error.Error_match err ->
            let actual_aliases =
              List.map ~f:Jay_error_location.show err.err_match_aliases
            in
            let actual_v =
              Jay.Jay_ast_pp.show_expr err.err_match_val.body
              |> String.substr_replace_all ~pattern:"\n" ~with_:" "
            in
            let a_actual_type, a_expected_type =
              ( Jay.Jay_ast_pp.show_on_type @@ err.err_match_actual,
                Jay.Jay_ast_pp.show_on_type @@ err.err_match_expected )
            in
            Match_error
              {
                m_value = (actual_aliases, actual_v);
                expected_type = a_expected_type;
                actual_type = a_actual_type;
              }
        | Sato_error.On_error.Error_value err ->
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
      let actual_err_loc =
        Bluejay_error_location.show @@ err.err_location
        |> String.substr_replace_all ~pattern:"\n" ~with_:""
      in
      let err_num = List.length err.err_errors in
      let actual_errs = err.err_errors in
      let transform_one_err_tnat (error : Sato_error.Bluejay_error.t) :
          Test_expect.error =
        match error with
        | Sato_error.Bluejay_error.Error_match err ->
            let actual_aliases =
              List.map ~f:Bluejay_error_location.show err.err_match_aliases
            in
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_match_val.body
              |> String.substr_replace_all ~pattern:"\n" ~with_:" "
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
        | Sato_error.Bluejay_error.Error_bluejay_type err ->
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_type_variable.body
              |> String.substr_replace_all ~pattern:"\n" ~with_:" "
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
        | Sato_error.Bluejay_error.Error_value err ->
            let actual_aliases =
              List.map ~f:Bluejay_error_location.show err.err_value_aliases
            in
            let actual_v =
              Bluejay.Bluejay_ast_pp.show_expr err.err_value_val.body
            in
            Value_error { v_value = (actual_aliases, actual_v) }
        | _ -> failwith "Expect no other error types!"
      in
      let errs = List.map ~f:transform_one_err_tnat actual_errs in
      {
        found_at_clause = actual_err_loc;
        number_of_errors = err_num;
        error_list = errs;
      }

let is_error_expected (actual : Sato_result.reported_error)
    (expected : Test_expect.t) : bool =
  let actual_error = errors_to_plain actual in
  let () = print_endline @@ Test_expect.show actual_error in
  let () = print_endline @@ Test_expect.show expected in
  Test_expect.equal expected actual_error

let test_one_file testname () =
  let program, odefa_inst_maps, on_to_odefa_maps_opt, ton_to_on_maps_opt =
    File_utils.read_source_sato testname
  in
  let config : Sato_args.t =
    {
      filename = testname;
      sato_mode = File_utils.mode_from_file testname;
      ddpa_c_stk = Sato_args.default_ddpa_c_stk;
      do_wrap = false;
      do_instrument = true;
      timeout = Some (Time.Span.of_int_sec 2);
      run_max_step = None;
    }
  in
  let errors_opt, _ =
    Main.main_from_program ~config odefa_inst_maps on_to_odefa_maps_opt
      ton_to_on_maps_opt program
  in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  match expectation with
  | None ->
      Alcotest.(check bool)
        "Expect no type errors" true
        (Option.is_none errors_opt)
  | Some expected -> (
      match errors_opt with
      | None -> Alcotest.(check bool) "Expect type error!" true false
      | Some error ->
          (* let () = failwith @@ string_of_bool @@ is_error_expected error expected in *)
          Alcotest.(check bool)
            "Type error matches expected" true
            (is_error_expected error expected))

let main test_path =
  let grouped_testfiles = group_all_files test_path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest.test_case testname `Quick @@ test_one_file testname) ))
  in
  Alcotest.run "Sato" grouped_tests ;
  ()

let () = main "test/sato"
(* let () = main "test-sato/playing-ground" *)
