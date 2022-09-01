open Core
open Dbmc

type config = {
  timeout : Time.Span.t option;
  engine : Global_config.engine;
  test_path : string;
  is_instrumented : bool;
  initial_stride : int;
}

let default_config =
  {
    timeout = Some (Time.Span.of_int_sec 5);
    engine = Global_config.E_dbmc;
    test_path = "test-sources";
    is_instrumented = false;
    initial_stride = Global_config.default_config.stride_init;
  }

let top_config = ref (Some default_config)

module Cmd_parser = struct
  open Cmdliner

  let timeout_conv =
    let parser s = Result.Ok (Scanf.sscanf s "%d" Time.Span.of_int_sec) in
    let printer oc s = Fmt.string oc @@ Time.Span.to_string_hum s in
    Arg.(conv (parser, printer))

  let timeout =
    let doc = "Timeout in seconds per test" in
    Arg.(
      value
      & opt (some timeout_conv) default_config.timeout
      & info [ "tm"; "timeout" ] ~docv:"TIMEOUT" ~doc)

  let no_timeout =
    let doc = "No timeout" in
    Arg.(value & flag & info [ "tmu"; "no-timeout" ] ~docv:"NOTIMEOUT" ~doc)

  let engine_conv =
    let parser s =
      if String.equal s "dbmc"
      then Result.Ok Global_config.E_dbmc
      else if String.equal s "ddse"
      then Result.Ok Global_config.E_ddse
      else Result.fail (`Msg "wrong engine")
    in
    let printer oc = function
      | Global_config.E_dbmc -> Fmt.string oc "dbmc"
      | Global_config.E_ddse -> Fmt.string oc "ddse"
    in
    Arg.(conv (parser, printer))

  let engine =
    let doc = "Symbolic interpreter engine." in
    Arg.(
      value
      & opt engine_conv default_config.engine
      & info [ "te"; "engine" ] ~docv:"ENGINE" ~doc)

  let test_path =
    let doc = "Path for test cases" in
    Arg.(
      value
      & opt string default_config.test_path
      & info [ "tp"; "test-path" ] ~docv:"TESTPATH" ~doc)

  let is_instrumented =
    let doc = "Instrument clauses." in
    Arg.(
      value & flag
      (* & opt bool false *)
      & info [ "ta"; "instrumented" ] ~docv:"INSTRUMENTED" ~doc)

  let initial_stride =
    let doc = "Initial stride to call SMT solver." in
    Arg.(
      value
      & opt int default_config.initial_stride
      & info [ "ts"; "stride" ] ~docv:"STRIDE" ~doc)

  let make_config timeout no_timeout engine test_path is_instrumented
      initial_stride =
    let timeout = if no_timeout then None else timeout in
    { timeout; engine; test_path; is_instrumented; initial_stride }

  let config =
    Term.(
      const make_config $ timeout $ no_timeout $ engine $ test_path
      $ is_instrumented $ initial_stride)
end

open Cmd_parser

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
              | `No when Jayil.File_utils.check_ext fullpath ->
                  (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

(* let int_option_checker : int option Alcotest.testable =
   let eq ii jj = match (ii, jj) with Some i, Some j -> i = j | _, _ -> true in
   Alcotest.testable Fmt.(Dump.option int) eq *)

let test_one_file test_config testname () =
  let open Lwt.Syntax in
  let is_instrumented = test_config.is_instrumented in

  let src = File_util.read_source ~is_instrumented testname in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  let config : Global_config.t =
    let filename = testname in
    {
      Global_config.default_config with
      filename;
      engine = test_config.engine;
      is_instrumented;
      stride_init = test_config.initial_stride;
      timeout = test_config.timeout;
    }
  in
  Dbmc.Log.init config ;
  match expectation with
  | None ->
      let* _, is_timeout = Main_lwt.search_input ~config src in
      prerr_endline "search_input, no expectation, end" ;
      Lwt.return
      @@ Alcotest.(check bool) "search_input: not timeout" false is_timeout
  | Some expectations ->
      Lwt_list.iter_s
        (fun (expectation : Test_expect.one_case) ->
          let config = { config with target = Id.Ident expectation.target } in
          match List.hd expectation.inputs with
          | Some inputs ->
              let* checked = Main_lwt.check_input ~config src inputs in
              prerr_endline "check_input, with expectation, end" ;
              Lwt.return
              @@ Alcotest.(check bool) "check_input: not timeout" false checked
          | None ->
              let* inputss, _ = Main_lwt.search_input ~config src in
              prerr_endline "search_input, end" ;
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
        Lwt_unix.with_timeout (Time.Span.to_sec t) (fun () ->
            test_one_file test_config testname ())
      with Lwt_unix.Timeout -> failwith "test_dbmc: timeout")
  | None -> test_one_file test_config testname ()

let main top_config =
  let grouped_testfiles = group_all_files top_config.test_path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest_lwt.test_case testname `Quick
              @@ test_one_file_lwt testname) ))
  in

  Lwt_main.run @@ Alcotest_lwt.run_with_args "DBMC" config grouped_tests ;
  Dbmc.Log.close () ;
  ()

let () =
  (ignore
  @@ Cmdliner.Cmd.(
       eval
       @@ v (info "test")
            Cmdliner.Term.(
              const (fun config -> top_config := Some config) $ config))) ;
  main (Option.value_exn !top_config)
