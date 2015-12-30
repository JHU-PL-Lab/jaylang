open Batteries;;
open BatOptParse.Opt;;
open Logger_utils;;

(** This logging option sets the global logging state of the application.  It
    "produces" a unit but, as each argument is parsed, has the side effect of
    configuring the logger. *)
let logging_option:unit BatOptParse.Opt.t =
  {
    (* Called whenever e.g. "--log debug" appears in the argument list *)
    option_set =
      (fun _ args ->
         let match_string_with_level level_str =
           match level_of_string level_str with
           | Some level -> level
           | None -> failwith ("Invalid log level \"" ^ level_str ^ "\".")
         in
         (match args with
          |[arg] ->
            (let (module_name_option,module_level) = 
               if BatString.exists arg "=" then
                 let (module_name,module_level) =
                   String.split ~by:arg "="
                 in (Some module_name,module_level) 
               else
                 (None,arg)
             in
             let level' = match_string_with_level module_level in
             match module_name_option with
             |Some(module_name) ->
               set_logging_level_for module_name level'
             |None ->
               set_default_logging_level level'
            )
          | _ -> raise @@ Option_error ("--log","Invalid argument")
         )
      )
    ;
    option_set_value = (fun _ -> ())
    ;
    option_get = (fun () -> Some())
    ;
    option_metavars = ["LOG_INSTR"]
    ;
    option_defhelp = Some("Sets the logging level.")
    ;
  };;

let disable_typechecking_option = BatOptParse.StdOpt.store_true ();;