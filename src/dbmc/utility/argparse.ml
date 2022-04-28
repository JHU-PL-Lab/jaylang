open Core
open Global_config

let ddpa_c_stk_parser : ddpa_c_stk Command.Arg_type.t =
  Command.Arg_type.create (function
    | "0ddpa" -> C_0ddpa
    | "1ddpa" -> C_1ddpa
    | "2ddpa" -> C_2ddpa
    | ddpa_c_string ->
        let k =
          String.chop_suffix_exn ddpa_c_string ~suffix:"ddpa" |> Int.of_string
        in
        C_kddpa k)

let log_level_parser : Logs.level Command.Arg_type.t =
  Command.Arg_type.create (function
    | "app" -> Logs.App
    | "error" -> Logs.Error
    | "warning" -> Logs.Warning
    | "info" -> Logs.Info
    | "debug" -> Logs.Debug
    | _ -> failwith "incorrect log level")

let target_parser : Id.t Command.Arg_type.t =
  Command.Arg_type.create (fun s -> Id.(Ident s))

let timeout_parser : Time.Span.t Command.Arg_type.t =
  Command.Arg_type.create (fun s -> Scanf.sscanf s "%d" Time.Span.of_int_sec)

let int_list_parser : int list Command.Arg_type.t =
  Command.Arg_type.create (fun s ->
      String.split_on_chars s ~on:[ ','; ';'; ' ' ]
      |> List.map ~f:(fun s -> Int.of_string s))

let engine_parser =
  Command.Arg_type.of_alist_exn [ ("dbmc", E_dbmc); ("ddse", E_ddse) ]

let parse_commandline_config () =
  let config = ref default_config in

  let command =
    Command.basic ~summary:"DBMC top to run ODEFA or NATODEFA file"
      (let open Command.Let_syntax in
      let%map_open target =
        flag "-t" (required target_parser) ~doc:"target_point"
      and ddpa_c_stk =
        flag "-c"
          (optional_with_default default_ddpa_c_stk ddpa_c_stk_parser)
          ~doc:"ddpa_concrete_stack"
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
      and filename = anon ("source_file" %: Filename.arg_type)
      and expected_inputs =
        flag "-i" (listed int_list_parser) ~doc:"expected_inputs by groups"
      and timeout =
        flag "-m" (optional timeout_parser) ~doc:"timeout in seconds"
      and stride_init =
        flag "-s"
          (optional_with_default default_config.stride_init int)
          ~doc:"check per steps (initial)"
      and stride_max =
        flag "-d"
          (optional_with_default default_config.stride_max int)
          ~doc:"check per steps (max)"
      and run_max_step = flag "-x" (optional int) ~doc:"check per steps"
      and engine =
        flag "-e" (optional_with_default E_dbmc engine_parser) ~doc:"engine"
      and debug_phi = flag "-p" no_arg ~doc:"output constraints"
      and debug_no_model = flag "-z" no_arg ~doc:"not output smt model"
      and debug_graph = flag "-g" no_arg ~doc:"output graphviz dot" in
      fun () ->
        let latter_option l1 l2 = Option.merge l1 l2 ~f:(fun _ y -> y) in

        let top_config =
          {
            ddpa_c_stk;
            filename;
            target;
            timeout;
            expected_inputs;
            stride_init;
            stride_max;
            run_max_step;
            engine;
            log_level;
            log_level_lookup = latter_option log_level log_level_lookup;
            log_level_solver = latter_option log_level log_level_solver;
            log_level_interpreter =
              latter_option log_level log_level_interpreter;
            debug_phi;
            debug_model = not debug_no_model;
            debug_graph;
          }
        in

        config := top_config)
  in
  Command.run command ;
  !config
