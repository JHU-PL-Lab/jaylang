(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type ddpa_c_stk = C_0ddpa | C_1ddpa | C_2ddpa | C_kddpa of int
[@@deriving show { with_path = false }]

type t = {
  ddpa_c_stk : ddpa_c_stk;
  target : Id.t;
  filename : Filename.t; [@printer String.pp]
  timeout : Time.Span.t option;
  expected_inputs : int list list;
  steps : int;
  run_max_step : int option;
  (* logger *)
  log_level : Logs.level option;
  log_level_lookup : Logs.level option;
  log_level_solver : Logs.level option;
  log_level_interpreter : Logs.level option;
  (* debug *)
  debug_phi : bool;
  debug_model : bool;
  debug_graph : bool;
}
[@@deriving show { with_path = false }]

let default_ddpa_c_stk = C_1ddpa

let default_config =
  {
    ddpa_c_stk = default_ddpa_c_stk;
    target = Id.(Ident "target");
    filename = "";
    timeout = None (* Time.Span.of_int_sec 60 *);
    steps = 100;
    expected_inputs = [];
    run_max_step = None;
    log_level = None;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
  }

let default_config_with ?(steps = default_config.steps) ~filename () : t =
  { default_config with filename; steps }

let check_wellformed_or_exit ast =
  let open Odefa_ast in
  try Ast_wellformedness.check_wellformed_expr ast
  with Ast_wellformedness.Illformedness_found ills ->
    print_endline "Program is ill-formed." ;
    ills
    |> List.iter ~f:(fun ill ->
           print_string "* " ;
           print_endline @@ Ast_wellformedness.show_illformedness ill) ;
    ignore @@ Stdlib.exit 1
