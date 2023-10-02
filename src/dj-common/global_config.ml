(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type analyzer = K_ddpa of int | K_cfa of int

type mode =
  | Dbmc_search
  | Dbmc_check of int option list
  | Dbmc_perf
  | Sato of File_utils.lang

type engine = E_dbmc | E_ddse
type encode_policy = Only_incremental | Always_shrink

type t = {
  (* basic *)
  target : Id.t;
  filename : Filename.t;
  (* mode *)
  mode : mode;
  stage : Stage.t;
  analyzer : analyzer;
  engine : engine;
  is_wrapped : bool;
  is_instrumented : bool;
  dump_instrumented : bool;
  (* tuning *)
  run_max_step : int option;
  timeout : Time_float.Span.t option;
  stride_init : int;
  stride_max : int;
  encode_policy : encode_policy;
  (* logger *)
  log_level : Logs.level option;
  log_level_lookup : Logs.level option;
  log_level_solver : Logs.level option;
  log_level_interpreter : Logs.level option;
  log_level_search : Logs.level option;
  log_level_complete_message : Logs.level option;
  log_level_perf : Logs.level option;
  global_logfile : string option;
  (* debug *)
  debug_phi : bool;
  debug_model : bool;
  debug_graph : bool;
  debug_interpreter : bool;
  is_check_per_step : bool;
}

let default_config =
  {
    target = Id.(Ident "target");
    filename = "";
    timeout = None (* Time.Span.of_int_sec 60 *);
    stride_init = 100;
    stride_max = 100;
    encode_policy = Only_incremental;
    analyzer = K_ddpa 1;
    stage = All_done;
    mode = Dbmc_search;
    run_max_step = None;
    engine = E_dbmc;
    is_wrapped = false;
    is_instrumented = false;
    dump_instrumented = false;
    log_level = None;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    log_level_search = None;
    log_level_complete_message = None;
    log_level_perf = None;
    global_logfile = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
    debug_interpreter = false;
    is_check_per_step = false;
  }

let default_sato_config =
  {
    default_config with
    mode = Sato File_utils.Jayil;
    is_wrapped = false;
    is_instrumented = true;
    dump_instrumented = false;
  }
