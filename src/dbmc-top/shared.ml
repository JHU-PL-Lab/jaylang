(* this module provide shared utilities by commandline and testing *)
open Core

exception GenComplete

type ddpa_c_stk = C_0ddpa | C_1ddpa | C_2ddpa | C_kddpa of int
[@@deriving show { with_path = false }]

type top_config = {
  ddpa_c_stk : ddpa_c_stk;
  log_level : Logs.level option;
  target : Dbmc.Id.t;
  filename : Filename.t; [@printer String.pp]
  timeout : Time.Span.t option;
  expected_inputs : int list list;
}
[@@deriving show { with_path = false }]

let default_ddpa_c_stk = C_1ddpa

(* let default_timeout = Time.Span.of_int_sec 60 *)
(* let default_target = Dbmc.Id.(Ident "target") *)

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

let dbmc_run program cfg =
  Dbmc.Log.init ~testname:cfg.filename ?log_level:cfg.log_level ();
  let inputs =
    Dbmc.Main.lookup_main ~testname:cfg.filename program
      (Dbmc.Id.to_ast_id cfg.target)
  in
  let inputs = List.hd_exn inputs in
  Format.printf "[%s]\n"
    (String.concat ~sep:"," @@ List.map ~f:string_of_int inputs);
  Dbmc.Log.close ()
(* ignore @@ raise GenComplete *)

let handle_config cfg =
  Format.printf "%a\n" pp_top_config cfg;
  let program =
    if String.is_suffix cfg.filename ~suffix:"natodefa" then
      let natast =
        Exn.handle_uncaught_and_exit (fun () ->
            In_channel.with_file cfg.filename
              ~f:Odefa_natural.On_parse.parse_program_raw)
      in
      Odefa_natural.On_to_odefa.translate natast
    else if String.is_suffix cfg.filename ~suffix:"odefa" then
      Exn.handle_uncaught_and_exit (fun () ->
          In_channel.with_file cfg.filename
            ~f:Odefa_parser.Parser.parse_program_raw)
    else
      failwith "file extension must be .odefa or .natodefa"
  in
  ignore @@ check_wellformed_or_exit program;
  dbmc_run program cfg
(* Format.printf "%a" Odefa_ast.Ast_pp.pp_expr program *)
