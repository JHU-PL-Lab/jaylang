open Core
open Dj_common

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
    test_path = "test/dbmc";
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
