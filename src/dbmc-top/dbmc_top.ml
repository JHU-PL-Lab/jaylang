(* this module is command-line parsing *)

open Core
open Shared

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

let target_parser : Dbmc.Id.t Command.Arg_type.t =
  Command.Arg_type.create (fun s -> Dbmc.Id.(Ident s))

let timeout_parser : Time.Span.t Command.Arg_type.t =
  Command.Arg_type.create (fun s -> Scanf.sscanf s "%d" Time.Span.of_int_sec)

let int_list_parser : int list Command.Arg_type.t =
  Command.Arg_type.create (fun s ->
      String.split_on_chars s ~on:[ ','; ';'; ' ' ]
      |> List.map ~f:(fun s -> Int.of_string s))

let command =
  Command.basic ~summary:"DBMC top to run ODEFA or NATODEFA file"
    (let open Command.Let_syntax in
    let%map_open target = flag "-t" (required target_parser) ~doc:"target_point"
    and ddpa_c_stk =
      flag "-c"
        (optional_with_default default_ddpa_c_stk ddpa_c_stk_parser)
        ~doc:"ddpa_concrete_stack"
    and log_level = flag "-l" (optional log_level_parser) ~doc:"logging_level"
    and filename = anon ("source_file" %: Filename.arg_type)
    and expected_inputs =
      flag "-i" (listed int_list_parser) ~doc:"expected_inputs by groups"
    and timeout =
      flag "-m" (optional timeout_parser) ~doc:"timeout in seconds"
    in
    let top_config =
      { ddpa_c_stk; log_level; filename; target; timeout; expected_inputs }
    in
    handle_config top_config;
    fun () -> ())

(* let command2 =
  Command.basic ~summary:"foo"
    (let open Command.Let_syntax in
    (* ignore @@ failwith "baz2"; *)
    let%map_open _ = anon ("bar" %: string) in
    fun () ->
      Backtrace.Exn.set_recording true;
      failwith "baz1") *)

let () = Command.run command
