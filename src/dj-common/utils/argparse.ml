open Core
open Global_config

let parse_analyzer str =
  let get_k suffix = String.chop_suffix_exn str ~suffix |> Int.of_string in
  if String.is_suffix str ~suffix:"ddpa"
  then Some (K_ddpa (get_k "ddpa"))
  else if String.is_suffix str ~suffix:"cfa"
  then Some (K_cfa (get_k "cfa"))
  else None

let parse_analyzer_exn str = parse_analyzer str |> Option.value_exn

let log_level_parser : Logs.level Command.Arg_type.t =
  Command.Arg_type.of_alist_exn
    [
      ("app", Logs.App);
      ("error", Logs.Error);
      ("warn", Logs.Warning);
      ("info", Logs.Info);
      ("debug", Logs.Debug);
    ]

let parse_id s = Id.(Ident s)

let timeout_parser : Time_float.Span.t Command.Arg_type.t =
  Command.Arg_type.create (fun s ->
      Scanf.sscanf s "%d" Time_float.Span.of_int_sec)

let input_spec_parser : Input_spec.t Command.Arg_type.t =
  Command.Arg_type.create (fun s ->
      String.split_on_chars s ~on:[ ',' ]
      |> List.map ~f:Input_spec.token_of_string)

let engine_parser =
  Command.Arg_type.of_alist_exn [ ("dbmc", E_dbmc); ("ddse", E_ddse) ]

let encode_policy_parser =
  Command.Arg_type.of_alist_exn
    [ ("inc", Only_incremental); ("shrink", Always_shrink) ]

let latter_option l1 l2 = Option.merge l1 l2 ~f:(fun _ y -> y)

let params_with ~config : Global_config.t Command.Param.t =
  let open Command.Let_syntax in
  let%map_open target =
    flag "-t" (optional (Command.Arg_type.create parse_id)) ~doc:"target_point"
  and filename = anon ("source_file" %: Filename_unix.arg_type)
  and stage =
    flag "-st"
      (optional_with_default config.stage
         (Command.Arg_type.create stage_of_str))
      ~doc:"stage"
  and engine =
    flag "-e" (optional_with_default config.engine engine_parser) ~doc:"engine"
  and is_wrapped = flag "-w" no_arg ~doc:"wrapped"
  and is_instrumented = flag "-a" no_arg ~doc:"instrumented"
  and dump_instrumented = flag "-ad" no_arg ~doc:"dump instrumented"
  and analyzer =
    flag "-aa"
      (optional_with_default config.analyzer
         (Command.Arg_type.create parse_analyzer_exn))
      ~doc:"ddpa_concrete_stack"
  and run_max_step = flag "-x" (optional int) ~doc:"check per steps"
  and timeout = flag "-m" (optional timeout_parser) ~doc:"timeout in seconds"
  and stride_init =
    flag "-si"
      (optional_with_default config.stride_init int)
      ~doc:"check per steps (initial)"
  and stride_max =
    flag "-sm"
      (optional_with_default config.stride_max int)
      ~doc:"check per steps (max)"
  and encode_policy =
    flag "-ep"
      (optional_with_default config.encode_policy encode_policy_parser)
      ~doc:"encode policy"
  and log_level =
    flag "-l"
      (optional log_level_parser)
      ~doc:"log level for all (can be override)"
  and log_level_lookup =
    flag "-ll" (optional log_level_parser) ~doc:"log level for lookup"
  and log_level_solver =
    flag "-ls" (optional log_level_parser) ~doc:"log level for solver"
  and log_level_interpreter =
    flag "-li" (optional log_level_parser) ~doc:"log level for interpreter"
  and log_level_search =
    flag "-ls2" (optional log_level_parser) ~doc:"log level for search"
  and log_level_complete_message =
    flag "-lm" (optional log_level_parser) ~doc:"log level for completemessage"
  and log_level_perf =
    flag "-lp" (optional log_level_parser) ~doc:"log level for perf"
  and debug_phi = flag "-dp" no_arg ~doc:"output constraints"
  and debug_no_model = flag "-dnm" no_arg ~doc:"not output smt model"
  and debug_graph = flag "-dg" no_arg ~doc:"output graphviz dot"
  and debug_interpreter = flag "-di" no_arg ~doc:"check the interpreter"
  and debug_check_per_step = flag "-dcs" no_arg ~doc:"check per step"
  and force_sato = flag "-s" no_arg ~doc:"sato mode"
  and expected_inputs =
    flag "-ei" (optional input_spec_parser) ~doc:"expected inputs"
  and expected_from_file = flag "-ef" no_arg ~doc:"expected from file" in
  let mode =
    if force_sato || Option.is_none target
    then Sato (File_utils.lang_from_file filename)
    else
      match expected_inputs with
      | Some inputs -> Dbmc_check inputs
      | None -> Dbmc_search
  in
  {
    target = (match target with Some t -> t | None -> Id.default_target);
    filename;
    engine;
    stage;
    is_wrapped;
    is_instrumented;
    dump_instrumented;
    mode;
    expected_from_file;
    analyzer;
    run_max_step;
    timeout;
    stride_init;
    stride_max;
    encode_policy;
    log_level;
    log_level_lookup = latter_option log_level log_level_lookup;
    log_level_solver = latter_option log_level log_level_solver;
    log_level_interpreter = latter_option log_level log_level_interpreter;
    log_level_search = latter_option log_level log_level_search;
    log_level_complete_message =
      latter_option log_level log_level_complete_message;
    log_level_perf = latter_option log_level log_level_perf;
    global_logfile = None;
    debug_phi;
    debug_model = not debug_no_model;
    debug_graph;
    debug_interpreter;
    debug_check_per_step;
  }

let parse_commandline ?config () =
  let config = match config with Some c -> c | None -> default_config in
  Std.param_of_command (params_with ~config) "DJ top"
