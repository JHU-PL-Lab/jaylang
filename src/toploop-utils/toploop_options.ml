open Batteries;;
open BatOptParse.Opt;;

open Logger_utils;;

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

module type Stack = Analysis_context_stack.Context_stack;;

let select_context_stack_option =
  (* This ref contains a module option.  If the option is None, no analysis is
     to be performed. *)
  let analysis_module_ref =
    ref (Some (module Analysis_single_element_stack.Stack : Stack))
  in
  {
    option_set = (fun option_name args ->
      match args with
      | [analysis_name] ->
        let analysis_module =
          try
            Toploop_ddpa.stack_from_name analysis_name
          with
          | Not_found ->
            raise @@ Option_error (option_name,
              Printf.sprintf "Invalid analysis name: %s" analysis_name)
        in
        analysis_module_ref := analysis_module
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun analysis_module_option ->
      analysis_module_ref := analysis_module_option
      )
    ;
    option_get = (fun () -> Some (!analysis_module_ref))
    ;
    option_metavars = ["ANALYSIS"]
    ;
    option_defhelp = Some("Selects an analysis (0ddpa,1ddpa,2ddpa,ddpaNR,none).")
    ;
  };;

let ddpa_logging_option =
  let logging_level = ref None in
  {
    option_set = (fun option_name args ->
      match args with
      | [level_name] ->
        let level =
          match level_name with
          | "none" -> Ddpa_graph_logger.Ddpa_log_none
          | "result" -> Ddpa_graph_logger.Ddpa_log_result
          | "all" -> Ddpa_graph_logger.Ddpa_log_all
          | _ -> raise @@ Option_error(option_name,
                    Printf.sprintf "Invalid DDPA logging level: %s" level_name)
        in
        logging_level := Some level
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun level -> logging_level := Some level)
    ;
    option_get = (fun () -> !logging_level)
    ;
    option_metavars = ["DDPA_LEVEL"]
    ;
    option_defhelp = Some("Selects a DDPA logging level (none,result,all).")
    ;       
  }
;;

let pdr_logging_option =
  let logging_level = ref None in
  {
    option_set = (fun option_name args ->
      match args with
      | [level_name] ->
        let level =
          match level_name with
          | "each-edge" ->
            Pds_reachability_logger_utils.Pds_reachability_log_each_edge
          | "each-call" ->
            Pds_reachability_logger_utils.Pds_reachability_log_each_call
          | "nothing" ->
            Pds_reachability_logger_utils.Pds_reachability_log_nothing
          | _ -> raise @@ Option_error(option_name,
                    Printf.sprintf "Invalid PDR logging level: %s" level_name)
        in
        logging_level := Some level
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun level -> logging_level := Some level)
    ;
    option_get = (fun () -> !logging_level)
    ;
    option_metavars = ["PDR_LEVEL"]
    ;
    option_defhelp =
      Some("Selects a PDR logging level (nothing,each-call,each-edge).")
    ;       
  }
;;
  