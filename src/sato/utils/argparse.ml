open Core
open Sato_args

let timeout_parser : Time_float.Span.t Command.Arg_type.t =
  Command.Arg_type.create (fun s ->
      Scanf.sscanf s "%d" Time_float.Span.of_int_sec)

let all_params : 'p Command.Param.t =
  let open Command.Let_syntax in
  let%map_open filename = anon ("source_file" %: Filename_unix.arg_type)
  (* and target =
          flag "-t" (required target_parser) ~doc:"target_point"
       and engine =
          flag "-e" (optional_with_default E_dbmc engine_parser) ~doc:"engine"
     and is_instrumented =
         flag "-a" (optional_with_default false bool) ~doc:"instrumented"
       and expected_inputs =
         flag "-i" (optional int_option_list_parser) ~doc:"expected inputs" *)
  and do_wrap = flag "-w" (optional_with_default true bool) ~doc:"enables wrap"
  and do_instrument =
    flag "-i" (optional_with_default true bool) ~doc:"performs instrumentation"
  and output_parsable =
    flag "-p"
      (optional_with_default false bool)
      ~doc:"output parsable .jil file"
  and ddpa_c_stk =
    flag "-c"
      (optional_with_default default_ddpa_c_stk
         Dj_common.Argparse.ddpa_c_stk_parser)
      ~doc:"ddpa_concrete_stack"
  and run_max_step = flag "-x" (optional int) ~doc:"check per steps"
  and timeout = flag "-m" (optional timeout_parser) ~doc:"timeout in seconds" in
  (* and stride_init =
           flag "-s"
             (optional_with_default default_config.stride_init int)
             ~doc:"check per steps (initial)"
         and stride_max =
           flag "-d"
             (optional_with_default default_config.stride_max int)
             ~doc:"check per steps (max)"
         and log_level =
           flag "-l"
             (optional log_level_parser)
             ~doc:"log level for all (can be override)"
         and log_level_lookup =
           flag "-ll" (optional log_level_parser) ~doc:"log level for lookup"
         and log_level_solver =
           flag "-lc" (optional log_level_parser) ~doc:"log level for solver"
         and log_level_interpreter =
           flag "-li" (optional log_level_parser) ~doc:"log level for interpreter"
         and debug_phi = flag "-p" no_arg ~doc:"output constraints"
         and debug_no_model = flag "-z" no_arg ~doc:"not output smt model"
         and debug_graph = flag "-g" no_arg ~doc:"output graphviz dot" in *)
  let latter_option l1 l2 = Option.merge l1 l2 ~f:(fun _ y -> y) in
  let sato_mode = Dj_common.File_utils.lang_from_file filename in
  {
    (* target; *)
    filename;
    (* engine; *)
    (* is_instrumented; *)
    (* expected_inputs; *)
    do_wrap;
    do_instrument;
    output_parsable;
    sato_mode;
    ddpa_c_stk;
    run_max_step;
    timeout;
    (* stride_init;
       stride_max;
       log_level;
       log_level_lookup = latter_option log_level log_level_lookup;
       log_level_solver = latter_option log_level log_level_solver;
       log_level_interpreter =
         latter_option log_level log_level_interpreter;
       debug_phi;
       debug_model = not debug_no_model;
       debug_graph; *)
  }

let parse_commandline_config () =
  let config = ref default_config in
  Dj_common.Command_util.parse_command all_params
    "Sato top to run ODEFA or NATODEFA file" config
