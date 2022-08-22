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
              | `No when File_utils.is_odefa_ext fullpath ->
                  (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

let is_error_expected 
  (actual : Sato_result.reported_error) 
  (expected : Test_expect.t) : bool = 
  let open Sato_result in
  match actual with
  | Odefa_error (err : Odefa_type_errors.t) ->
    let actual_err_loc = Odefa_error_location.show @@ err.err_location in
    let expected_err_loc = expected.found_at_clause in
    let actual_errors = err.err_errors in
    let actual_err_num = 
      List.length actual_errors
    in
    let expected_err_num = expected.number_of_errors in 
    let check_1 = 
      String.equal actual_err_loc expected_err_loc && actual_err_num = expected_err_num 
    in 
    (* let () = failwith @@ string_of_bool @@ check_1 in *)
    if check_1 then
      let combined = List.zip_exn actual_errors expected.error_list in
      let folder acc (actual_err, expected_err) =
        match (actual_err, expected_err) with
        | Sato_error.Odefa_error.Error_match err, Test_expect.Match_error err' ->
          let actual_aliases = 
            List.map ~f:(fun (Odefa_ast.Ast.Ident i) -> i) err.err_match_aliases 
          in
          let actual_v =
            Odefa_ast.Ast_pp_brief.show_clause_body err.err_match_val
          in
          let (expected_aliases, expected_v) = 
            err'.m_value
          in
          let c_1 = List.equal String.equal actual_aliases expected_aliases in
          let c_2 = String.equal actual_v expected_v in
          let check_2 = c_1 && c_2 in
          (* let () = print_endline @@ List.to_string ~f:(fun x -> x) actual_aliases in
          let () = print_endline @@ List.to_string ~f:(fun x -> x) expected_aliases in *)
          (* let () = print_endline @@ actual_v in *)
          (* let () = print_endline @@ expected_v in *)
          (* let () = failwith @@ string_of_bool @@ check_2 in *)
          if check_2 then
            let (a_actual_type, a_expected_type) = 
              (Odefa_ast.Ast_pp.show_type_sig @@ err.err_match_actual, 
               Odefa_ast.Ast_pp.show_type_sig @@ err.err_match_expected)
            in 
            let (e_actual_type, e_expected_type) = 
              err'.actual_type, err'.expected_type
            in
            String.equal a_actual_type e_actual_type &&
            String.equal a_expected_type e_expected_type
          else
            false
        | Sato_error.Odefa_error.Error_value err, Test_expect.Value_error err' ->
          let actual_aliases = 
            List.map ~f:(fun (Odefa_ast.Ast.Ident i) -> i) err.err_value_aliases 
          in
          let actual_v =
            Odefa_ast.Ast_pp.show_clause_body err.err_value_val
          in
          let (expected_aliases, expected_v) = 
            err'.v_value
          in
          let c_1 = List.equal String.equal actual_aliases expected_aliases in
          let c_2 = String.equal actual_v expected_v in
          let check_2 = c_1 && c_2 in
          (* let () = print_endline @@ List.to_string ~f:(fun x -> x) actual_aliases in
          let () = print_endline @@ List.to_string ~f:(fun x -> x) expected_aliases in *)
          (* let () = print_endline @@ actual_v in *)
          (* let () = print_endline @@ expected_v in *)
          (* let () = failwith @@ string_of_bool @@ check_2 in *)
          check_2
        | _ -> acc 
      in
      List.fold ~init:true ~f:folder combined
    else false
  | Natodefa_error _ -> failwith "Natodefa TBI!"

let test_one_file testname () =
  let (program, odefa_inst_maps, on_to_odefa_maps_opt, _) = 
    File_utils.read_source_sato testname 
  in
  let config : Sato_args.t = 
    { filename = testname;
      is_natodefa = File_utils.is_natodefa_ext testname;
      ddpa_c_stk = Sato_args.default_ddpa_c_stk;
      timeout = Some (Time.Span.of_int_sec 10);
      run_max_step = None;
    }
  in
  let errors_opt = 
    Main.main_from_program 
      ~config:config odefa_inst_maps on_to_odefa_maps_opt None program 
  in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  match expectation with
  | None ->
    Alcotest.(check bool) "Expect no type errors" true (Option.is_none errors_opt)
  | Some expected -> 
    match errors_opt with
    | None -> 
      Alcotest.(check bool) "Expect type error!" true false
    | Some error ->
      (* let () = failwith @@ string_of_bool @@ is_error_expected error expected in *)
      Alcotest.(check bool) 
        "Type error matches expected" true (is_error_expected error expected)

let main test_path =
  let grouped_testfiles = group_all_files test_path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest.test_case testname `Quick
              @@ test_one_file testname) ))
  in
  Alcotest.run "Sato" grouped_tests ;
  ()

let () = 
  main "test-sato/odefa-types"