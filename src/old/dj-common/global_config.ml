(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type analyzer = K_ddpa of int | K_cfa of int

type mode =
  | Dbmc_search
  | Dbmc_check of Input_spec.t
  | Dbmc_perf
  | Sato of File_utils.lang

type engine = E_dbmc | E_ddse
type encode_policy = Only_incremental | Always_shrink

type stage = Argparse | Load_file | State_init | Lookup | All_done
[@@deriving variants, equal]

type dump_level = No_Dump | Instrumented | All [@@deriving variants, equal]

let stage_of_str str =
  match str with
  | "argparse" | "ap" -> Argparse
  | "load_file" | "lf" -> Load_file
  | "state_init" | "si" -> State_init
  | "lookup" | "lu" -> Lookup
  | "all_done" | "ad" | "all" | _ -> All_done

let dump_level_of_str str =
  match str with
  | "instrumented" | "ins" -> Instrumented
  | "all" -> All
  | "none" | _ -> No_Dump

type t = {
  (* basic *)
  target : Id.t;
  filename : Filename.t;
  (* mode *)
  mode : mode;
  stage : stage;
  analyzer : analyzer;
  engine : engine;
  is_wrapped : bool;
  is_instrumented : bool;
  expected_from_file : bool;
  dump_level : dump_level;
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
  log_level_concolic : Logs.level option;
  log_level_search : Logs.level option;
  log_level_complete_message : Logs.level option;
  log_level_perf : Logs.level option;
  global_logfile : string option;
  (* debug *)
  debug_phi : bool;
  debug_model : bool;
  debug_graph : bool;
  debug_interpreter : bool;
  debug_check_per_step : bool;
}

let default_config =
  {
    target = Id.(Ident "target");
    filename = "";
    (* timeout = None  *)
    timeout = Some (Time_float.Span.of_int_sec 5);
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
    expected_from_file = false;
    dump_level = No_Dump;
    log_level = None;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    log_level_concolic = None;
    log_level_search = None;
    log_level_complete_message = None;
    log_level_perf = None;
    global_logfile = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
    debug_interpreter = false;
    debug_check_per_step = false;
  }

let default_sato_config =
  {
    default_config with
    analyzer = K_ddpa 1;
    mode = Sato File_utils.Jayil;
    is_wrapped = false;
    is_instrumented = true;
    dump_level = No_Dump;
  }

let default_sato_test_config =
  {
    default_config with
    analyzer = K_ddpa 1;
    mode = Sato File_utils.Jayil;
    timeout = Some (Time_float.Span.of_int_sec 2);
    is_wrapped = true;
    is_instrumented = true;
    dump_level = No_Dump;
  }

let with_filename filename = { default_config with filename }

let with_expect (expect : Test_expect.one_case) config =
  let config' = { config with target = Id.Ident expect.target } in
  if Input_spec.is_no_spec expect.inputs
  then config'
  else { config' with mode = Dbmc_check expect.inputs }

let load_expect config =
  let expect = File_utils.load_expect_d config.filename in
  with_expect (List.hd_exn expect) config

let read_source config =
  (* let target_var = Jayil.Ast.Var (config.target, None) in *)
  File_utils.read_source ~do_wrap:config.is_wrapped
    ~do_instrument:config.is_instrumented (*~consts:[ target_var ]*) config.filename
