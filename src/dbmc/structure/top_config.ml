(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type ddpa_c_stk = C_0ddpa | C_1ddpa | C_2ddpa | C_kddpa of int
[@@deriving show { with_path = false }]

type t = {
  ddpa_c_stk : ddpa_c_stk;
  log_level : Logs.level option;
  target : Id.t;
  filename : Filename.t; [@printer String.pp]
  timeout : Time.Span.t option;
  steps : int;
  expected_inputs : int list list;
  (* debug *)
  debug_phi : bool;
  debug_model : bool;
  debug_graph : bool;
  run_max_step : int option;
}
[@@deriving show { with_path = false }]

let default_ddpa_c_stk = C_1ddpa

let default_config =
  {
    ddpa_c_stk = default_ddpa_c_stk;
    log_level = None;
    target = Id.(Ident "target");
    filename = "";
    timeout = None (* Time.Span.of_int_sec 60 *);
    steps = 1;
    expected_inputs = [];
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
    run_max_step = None;
  }

let default_config_with ?(steps = 500) ~filename : t =
  { default_config with filename; steps }

let check_wellformed_or_exit ast =
  let open Odefa_ast in
  try Ast_wellformedness.check_wellformed_expr ast
  with Ast_wellformedness.Illformedness_found ills ->
    print_endline "Program is ill-formed.";
    ills
    |> List.iter ~f:(fun ill ->
           print_string "* ";
           print_endline @@ Ast_wellformedness.show_illformedness ill);
    ignore @@ Stdlib.exit 1
