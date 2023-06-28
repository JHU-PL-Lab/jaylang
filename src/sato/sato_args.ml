open Core

type t = {
  (* basic *)
  filename : Filename.t; [@printer String.pp]
  sato_mode : Dj_common.File_utils.lang;
  (* analysis *)
  ddpa_c_stk : Dj_common.Global_config.ddpa_c_stk;
  (* tuning *)
  do_wrap : bool;
  do_instrument : bool;
  output_parsable : bool;
  run_max_step : int option;
  timeout : Time_float.Span.t option;
}
[@@deriving show]

let default_ddpa_c_stk = Dj_common.Global_config.C_1ddpa

let default_config =
  {
    filename = "";
    sato_mode = Dj_common.File_utils.Jayil;
    ddpa_c_stk = default_ddpa_c_stk;
    do_wrap = true;
    do_instrument = true;
    output_parsable = false;
    timeout = None (* Time.Span.of_int_sec 60 *);
    run_max_step = None;
  }

let sato_to_dbmc_config (sato_config : t) : Dj_common.Global_config.t =
  (* Extract basic configuration from sato args *)
  let open Dj_common in
  let filename = sato_config.filename in
  let ddpa_ver = sato_config.ddpa_c_stk in
  let max_step = sato_config.run_max_step in
  let timeout = sato_config.timeout in
  let open Dj_common.Global_config in
  {
    Global_config.default_config with
    filename;
    engine = E_dbmc;
    is_instrumented = false;
    mode = Sato;
    ddpa_c_stk = ddpa_ver;
    run_max_step = max_step;
    timeout;
  }
