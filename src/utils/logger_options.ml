open Batteries;;
open BatOptParse.Opt;;
open Logger_utils;;

let logging_option:logging_config  BatOptParse.Opt.t =
  {
    (* Called whenever e.g. "--log debug" appears in the argument list *)
    option_set =
      (fun _ args ->
         (match args with
          | [arg] ->
            (let (module_name_option,module_level) = 
               if BatString.exists arg "=" then
                 let (module_name,module_level) = String.split arg ~by:"="
                 in (Some module_name,module_level) 
               else
                 (None,arg)
             in
             let Logging_config(default_level, level_map) = !logging_config_global in
             match module_name_option with
             | Some(module_name) ->
               logging_config_global :=
                 Logging_config(default_level,
                                String_map.add
                                  module_name
                                  (match_string_with_level module_level)
                                  level_map)
             | None ->
               logging_config_global :=
                 Logging_config(
                   (match_string_with_level module_level), level_map)
            )
          | _ -> raise @@ Option_error ("--log","Invalid argument")
         )
      )
    ;
    option_set_value = (fun data -> logging_config_global := data)
    ;
    option_get = (fun () -> Some(!logging_config_global))
    ;
    option_metavars = ["LOG_INSTR"]
    ;
    option_defhelp = Some("Sets the logging level (e.g. \"debug\").")
    ;
  };;
