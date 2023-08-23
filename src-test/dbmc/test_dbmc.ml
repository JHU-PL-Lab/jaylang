open Core
open Dj_common
open Dbmc
open Test_dbmc_cmd
open Test_dbmc_cmd.Cmd_parser

let test_one_file test_config testname () =
  let open Lwt.Syntax in
  let is_instrumented = test_config.is_instrumented in

  let src = File_utils.read_source ~do_instrument:is_instrumented testname in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  let config : Global_config.t =
    let filename = testname in
    {
      Global_config.default_config with
      filename;
      analyzer = test_config.analyzer;
      engine = test_config.engine;
      is_instrumented;
      stride_init = test_config.initial_stride;
      timeout = test_config.timeout;
    }
  in
  Dj_common.Log.init config ;
  match expectation with
  | None ->
      let* { is_timeout; _ } = Main.main_lwt ~config src in
      prerr_endline "search_input, no expectation, end" ;
      Lwt.return
      @@ Alcotest.(check bool) "search_input: not timeout" false is_timeout
  | Some expectations ->
      Lwt_list.iter_s
        (fun (expectation : Test_expect.one_case) ->
          let config = { config with target = Id.Ident expectation.target } in
          match List.hd expectation.inputs with
          | Some inputs ->
              let config =
                { config with mode = Global_config.Dbmc_check inputs }
              in
              let* _ = Main.main_lwt ~config src in
              let checked = false in
              Lwt.return
              @@ Alcotest.(check bool) "check_input: not timeout" false checked
          | None ->
              let* { inputss; _ } = Main.main_lwt ~config src in
              let () =
                match List.hd inputss with
                | Some _inputs ->
                    Alcotest.(check bool) "shouldn't have result" true false
                | None ->
                    Alcotest.(check int)
                      "equal" 0
                      (List.length expectation.inputs)
              in
              Lwt.return_unit)
        expectations

let test_one_file_lwt testname _switch test_config =
  match test_config.timeout with
  | Some t -> (
      try%lwt
        Lwt_unix.with_timeout (Time_float.Span.to_sec t) (fun () ->
            test_one_file test_config testname ())
      with Lwt_unix.Timeout -> failwith "test_dbmc: timeout")
  | None -> test_one_file test_config testname ()

let main top_config =
  let grouped_tests =
    Directory_utils.map_in_groups
      ~f:(fun _ test_name test_path ->
        Alcotest_lwt.test_case test_name `Quick @@ test_one_file_lwt test_path)
      top_config.test_path
  in

  Lwt_main.run @@ Alcotest_lwt.run_with_args "DBMC" config grouped_tests ;
  Dj_common.Log.close ()

let () =
  (ignore
  @@ Cmdliner.Cmd.(
       eval
       @@ v (info "test")
            Cmdliner.Term.(
              const (fun config -> top_config := Some config) $ config))) ;
  main (Option.value_exn !top_config)
