(**
   A simple module for logging in Odefa.
*)

open Batteries;;

module String_map = BatMap.Make(BatString);;

type level = [`warn|`trace|`debug|`info|`error|`fatal|`always];;

type logging_config = Logging_config of level * level String_map.t;;

exception Option_error of string * string;;

let match_string_with_level level_string = 
  match level_string with
  | "trace" -> `trace
  | "debug" -> `debug
  | "info" -> `info
  | "warn" -> `warn
  | "error" -> `error
  | "fatal" -> `fatal
  | "always" -> `always
  | _ -> failwith ("Invalid log level `" ^ level_string ^ "'.")
;;

let match_string_with_bool boolean_string = 
  match boolean_string with
  | "true" -> true
  | "false" -> false
  | _ -> failwith ("Invalid boolean for typecheck'" ^ boolean_string ^ " '.")


let extract_map log_config = match log_config with
  | Logging_config(_,map) -> map;;

let extract_default log_config = match log_config with
  | Logging_config(default,_) -> default;;

let logging_config_global = ref @@ Logging_config(`warn, String_map.empty);;

let type_check_global = ref true;;

let default_level = ref `warn ;;

let level_map = ref String_map.empty;;

let update_levels () = 
  default_level := (extract_default (!logging_config_global));
  level_map := (extract_map (!logging_config_global))
;;

let level_for prefix =
  update_levels (); 
  if String_map.mem prefix !level_map
  then  (String_map.find prefix !level_map)
  else
    !default_level
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
