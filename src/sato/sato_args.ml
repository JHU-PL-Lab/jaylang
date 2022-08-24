open Core

type mode = Odefa | Natodefa | Typed_natodefa
[@@deriving show]

type t = {
  (* basic *)
  filename : Filename.t; [@printer String.pp]
  sato_mode : mode;
  (* analysis *)
  ddpa_c_stk : Dbmc.Global_config.ddpa_c_stk;
  (* tuning *)
  run_max_step : int option;
  timeout : Time.Span.t option;
}
[@@deriving show]

let default_ddpa_c_stk = Dbmc.Global_config.C_1ddpa

let default_config =
  {
    filename = "";
    sato_mode = Odefa;
    ddpa_c_stk = default_ddpa_c_stk;
    timeout = None (* Time.Span.of_int_sec 60 *);
    run_max_step = None;
  }