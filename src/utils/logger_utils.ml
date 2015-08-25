(*
 * A Simple Logging Module
 *
 * Features
 * --------
 *
 * 1. Always logs to the stderr, which is the best thing to do [1]. Note that the
 *    article in [1] says to log to stdout, but currently the interpreter is only
 *    run in interactive mode, so stdout is already taken and we log the next best
 *    thing, which is stderr.
 *
 * 2. Handles different log levels.
 *
 * 3. Is contextualized by the module and different log levels per module can
 *    be configured.
 *
 * 4. Is a thin layer around BatLog.
 *
 * Usage
 * -----
 *
 * 1. In `_oasis`, for the library that requires logging, add `tiny-bang-utils` to
 *    the `BuildDepends` section.
 *
 * 2. In the module that needs logging, on the top of the file, create a logger
 *    with a name that identifies the module. For example, in `tiny_bang_toploop.ml`:
 *
 *    ```ocaml
 *    let logger = Logger.make_logger "Toploop"
 *    ```
 *
 * 3. Add log entries. For example:
 *
 *    ```ocaml
 *    logger `debug "chunky tempeh!"
 *    ```
 *
 *    Allowed log levels are: `trace | `debug | `info | `warn | `error
 *                            | `fatal | `always.
 *
 * 4. (Optional) Log levels can be selected for the entire executable or on a
 *               per-module basis using command line arguments. Refer to the
 *               `README.md' for more information.
 *
 *
 * [1]: http://12factor.net/logs
 *)

open Batteries

let rec parse_command_line_parameters arguments 
    result 
    module_log_level_function 
    default_log_level_function =
  match arguments with
  | [] -> result
  | [single_parameter] -> if single_parameter = "--log" then
      failwith "Missing log level after `--log'."
    else
      result
  | flag :: argument :: arguments_tail ->
    if flag = "--log" then
      if BatString.exists argument "=" then
        module_log_level_function argument result
      else
        default_log_level_function argument result
    else
      parse_command_line_parameters (argument :: arguments_tail) 
        result 
        module_log_level_function 
        default_log_level_function
;;

let default_level =
  parse_command_line_parameters (BatArray.to_list Sys.argv)
    "warn"
    (fun argument result-> result)
    (fun argument result-> argument)
;;

let level_map =  
  parse_command_line_parameters (BatArray.to_list Sys.argv)
    BatMap.empty
    (fun argument result->
       let (module_name, level_string) = BatString.split argument "=" in
       BatMap.add module_name level_string result)
    (fun argument result-> result)
;;

let level_for prefix =
  let level_string =
    if BatMap.mem prefix level_map then
      BatMap.find prefix level_map
    else
      default_level
  in match level_string with
  | "trace" -> `trace
  | "debug" -> `debug
  | "info" -> `info
  | "warn" -> `warn
  | "error" -> `error
  | "fatal" -> `fatal
  | "always" -> `always
  | _ -> failwith ("Invalid log level `" ^ level_string ^ "'.")
;;

let make_logger prefix level message =
  BatLog.Easy.level := level_for prefix;
  BatLog.Easy.log level ("[" ^ prefix ^ "]: " ^ message);
  flush stderr
;;

let bracket_log logger level pre_message post_message_fn thunk =
  logger level pre_message;
  let value = thunk () in
  logger level (pre_message ^ "\n  : " ^ post_message_fn value);
  value
;;