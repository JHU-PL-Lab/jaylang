open Batteries;;

module String_map = BatMap.Make(BatString);;

type level = [`trace|`debug|`info|`warn|`error|`fatal|`always];;

let level_of_string level_string =
  match level_string with
  | "trace" -> Some `trace
  | "debug" -> Some `debug
  | "info" -> Some `info
  | "warn" -> Some `warn
  | "error" -> Some `error
  | "fatal" -> Some `fatal
  | "always" -> Some `always
  | _ -> None
;;

let default_level = ref `warn ;;

let level_map = ref String_map.empty;;

let set_default_logging_level level = default_level := level;;

let set_logging_level_for module_name level =
  level_map := String_map.add module_name level !level_map
;;

let level_for prefix =
  if String_map.mem prefix !level_map
  then (String_map.find prefix !level_map)
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
