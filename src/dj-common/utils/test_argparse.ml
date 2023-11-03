open Core

type config = {
  analyzer : Global_config.analyzer;
  engine : Global_config.engine;
  is_instrumented : bool;
  timeout : Time_float.Span.t option;
  test_path : string;
  initial_stride : int;
}

let default_config =
  {
    analyzer = Global_config.default_config.analyzer;
    engine = Global_config.default_config.engine;
    is_instrumented = false;
    timeout = Some (Time_float.Span.of_int_sec 5);
    test_path = "test/dbmc";
    initial_stride = Global_config.default_config.stride_init;
  }

let top_config = ref (Some default_config)

module Cmd_parser = struct
  open Cmdliner

  let analyzer_conv =
    let parser s =
      match Argparse.parse_analyzer s with
      | Some aa -> Result.Ok aa
      | None -> Result.fail (`Msg "wrong analyzer")
    in
    let printer ff = function
      | Global_config.K_ddpa k -> Fmt.pf ff "%dddpa" k
      | Global_config.K_cfa k -> Fmt.pf ff "%dcfa" k
    in
    Arg.(conv (parser, printer))

  let analyzer =
    let doc = "Program Analyzer." in
    Arg.(
      value
      & opt analyzer_conv default_config.analyzer
      & info [ "aa"; "analyzer" ] ~docv:"ANALYZER" ~doc)

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

  let is_instrumented =
    let doc = "Instrument clauses." in
    Arg.(
      value & flag
      (* & opt bool false *)
      & info [ "ta"; "instrumented" ] ~docv:"INSTRUMENTED" ~doc)

  let timeout_conv =
    let parser s = Result.Ok (Scanf.sscanf s "%d" Time_float.Span.of_int_sec) in
    let printer oc s = Fmt.string oc @@ Time_float.Span.to_string_hum s in
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

  let test_path =
    let doc = "Path for test cases" in
    Arg.(
      value
      & opt string default_config.test_path
      & info [ "tp"; "test-path" ] ~docv:"TESTPATH" ~doc)

  let initial_stride =
    let doc = "Initial stride to call SMT solver." in
    Arg.(
      value
      & opt int default_config.initial_stride
      & info [ "ts"; "stride" ] ~docv:"STRIDE" ~doc)

  let make_config analyzer engine is_instrumented timeout no_timeout test_path
      initial_stride =
    let timeout = if no_timeout then None else timeout in
    { analyzer; engine; is_instrumented; timeout; test_path; initial_stride }
end

open Cmd_parser

let config : config Cmdliner.Term.t =
  Cmdliner.Term.(
    const make_config $ analyzer $ engine $ is_instrumented $ timeout
    $ no_timeout $ test_path $ initial_stride)

let parse_test_commandline () =
  (ignore
  @@ Cmdliner.Cmd.(
       eval
       @@ v (info "test")
            Cmdliner.Term.(
              const (fun config -> top_config := Some config) $ config))) ;
  Option.value_exn !top_config
