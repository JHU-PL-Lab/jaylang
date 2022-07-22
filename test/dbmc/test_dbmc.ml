open Core
open Dbmc

type config = {
  timeout : Time.Span.t option;
  engine : Global_config.engine;
  test_path : string;
  is_instrumented : bool;
  initial_stride : int;
}

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
      & opt (some timeout_conv) (Some (Time.Span.of_int_sec 5))
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
      & opt engine_conv Global_config.E_dbmc
      & info [ "te"; "engine" ] ~docv:"ENGINE" ~doc)

  let test_path =
    let doc = "Path for test cases" in
    Arg.(
      value & opt string "test-sources"
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
      & opt int Global_config.default_config.stride_init
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
              | `No when File_util.is_odefa_ext fullpath ->
                  (fullpath :: acc_f, acc_p)
              | `No -> (acc_f, acc_p)
              | `Unknown -> (acc_f, acc_p)))
        dir
    in
    (dir, List.sort acc_f ~compare:String.compare) :: acc_p
  in
  loop dir

let int_option_checker : int option Alcotest.testable =
  let eq ii jj = match (ii, jj) with Some i, Some j -> i = j | _, _ -> true in
  Alcotest.testable Fmt.(Dump.option int) eq

let test_one_file test_config testname () =
  let is_instrumented = test_config.is_instrumented in
  let src = File_util.read_source ~is_instrumented testname in
  let expectation = Test_expect.load_sexp_expectation_for testname in
  let config : Global_config.t =
    let filename = testname in
    let timeout = Some (Time.Span.of_int_sec 5) in
    {
      Global_config.default_config with
      filename;
      engine = test_config.engine;
      is_instrumented;
      stride_init = test_config.initial_stride;
      timeout;
    }
  in
  Dbmc.Log.init config ;
  match expectation with
  | None ->
      let _ = Main.search_input ~config src in
      Alcotest.(check unit) "unit" () ()
  | Some expectations ->
      List.iter expectations ~f:(fun expectation ->
          let config = { config with target = Id.Ident expectation.target } in
          match List.hd expectation.inputs with
          | Some inputs ->
              let checked = Main.check_input ~config src inputs in
              ignore @@ Lwt.return (Alcotest.fail "immediate1") ;

              Alcotest.(check bool) "equal" true checked
          | None -> (
              let inputss = Main.search_input ~config src in
              ignore @@ failwith "immediate2" ;
              ignore @@ Alcotest.fail "immediate2-1" ;
              match List.hd inputss with
              | Some inputs ->
                  let expected_inputs = List.hd_exn expectation.inputs in
                  Alcotest.(check (list int_option_checker))
                    "equal" inputs expected_inputs ;
                  Alcotest.(check (list int_option_checker))
                    "equal" inputs expected_inputs
              | None ->
                  ignore @@ failwith "immediate3" ;
                  Alcotest.(check int)
                    "equal" 0
                    (List.length expectation.inputs)))
(* exception Timeout *)

(* let sigalrm_handler = Caml.Sys.Signal_handle (fun _ -> raise Timeout) *)

(* TODO: it works strangely. Maybe because this timeout won't break a Lwt.t. *)
(* let timeout f arg time =
   let old_behavior = Caml.Sys.signal Caml.Sys.sigalrm sigalrm_handler in
   let reset_sigalrm () = Caml.Sys.set_signal Caml.Sys.sigalrm old_behavior in
   ignore (Unix.alarm time) ;
   try
     let res = f arg in
     reset_sigalrm () ;
     res
   with exc ->
     reset_sigalrm () ;
     raise exc *)

let test_one_file_lwt testname _switch test_config =
  match test_config.timeout with
  | Some t -> (
      try%lwt
        Lwt_unix.with_timeout (Time.Span.to_sec t) (fun () ->
            Lwt.return (test_one_file test_config testname ()))
      with Lwt_unix.Timeout -> failwith "timeoutout")
  | None -> Lwt.return (test_one_file test_config testname ())

(* let test_one_file_lwt testname _switch () =
   Lwt.return (test_one_file testname ()) *)

(* let test_one_file_timed testname () = test_one_file testname () *)
(* try timeout (test_one_file testname) () 5 with Timeout -> raise Timeout *)

let main top_config =
  let grouped_testfiles = group_all_files top_config.test_path in
  let grouped_tests =
    List.map grouped_testfiles ~f:(fun (group_name, test_names) ->
        ( group_name,
          List.map test_names ~f:(fun testname ->
              Alcotest_lwt.test_case testname `Quick
              @@ test_one_file_lwt testname
              (* Alcotest.test_case testname `Quick @@ test_one_file_timed testname *))
        ))
  in

  (* Alcotest.run "DBMC" grouped_tests; *)
  Lwt_main.run
  @@ (* Alcotest_lwt.run "DBMC" grouped_tests ; *)
  Alcotest_lwt.run_with_args "DBMC" config grouped_tests ;
  Dbmc.Log.close () ;
  ()

let top_config = ref None

let () =
  (ignore
  @@ Cmdliner.Cmd.(
       eval
       @@ v (info "test")
            Cmdliner.Term.(
              const (fun config -> top_config := Some config) $ config))) ;
  main (Option.value_exn !top_config)
