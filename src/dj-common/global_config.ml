(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type ddpa_c_stk = C_0ddpa | C_1ddpa | C_2ddpa | C_kddpa of int
and engine = E_dbmc | E_ddse
and mode = Dbmc_search | Dbmc_check of int option list | Sato
and encode_policy = Only_incremental | Always_shrink

and t = {
  (* basic *)
  target : Id.t;
  filename : Filename.t; [@printer String.pp]
  (* mode *)
  engine : engine;
  is_instrumented : bool;
  mode : mode;
  (* analysis *)
  ddpa_c_stk : ddpa_c_stk;
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
[@@deriving show { with_path = false }]

let default_ddpa_c_stk = C_1ddpa

let default_config =
  {
    ddpa_c_stk = default_ddpa_c_stk;
    target = Id.(Ident "target");
    filename = "";
    timeout = None (* Time.Span.of_int_sec 60 *);
    stride_init = 100;
    stride_max = 100;
    encode_policy = Only_incremental;
    mode = Dbmc_search;
    run_max_step = None;
    engine = E_dbmc;
    is_instrumented = false;
    log_level = Some Logs.Debug;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    log_level_search = Some Logs.Debug;
    log_level_complete_message = None;
    log_level_perf = None;
    global_logfile = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
    debug_interpreter = false;
    is_check_per_step = false;
  }
