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
          begin
            match analysis_name with
            | "cba0" ->
              Some (module Analysis_unit_stack.Stack : Stack)
            | "cba1" ->
              Some (module Analysis_single_element_stack.Stack : Stack)
            | "cba2" ->
              Some (module Analysis_two_element_stack.Stack : Stack)
            | "cbanr" ->
              Some (module Analysis_nonrepeating_stack.Stack : Stack)
            | "none" -> None
            | _ -> raise @@ Option_error (option_name,
                      Printf.sprintf "Invalid analysis name: %s" analysis_name)
          end
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
    option_defhelp = Some("Selects an analysis (cba0,cba1,cba1,cbanr,none).")
    ;
  };;

let cba_logging_option =
  let logging_level = ref None in
  {
    option_set = (fun option_name args ->
      match args with
      | [level_name] ->
        let level =
          match level_name with
          | "none" -> Cba_graph_logger.Cba_log_none
          | "result" -> Cba_graph_logger.Cba_log_result
          | "all" -> Cba_graph_logger.Cba_log_all
          | _ -> raise @@ Option_error(option_name,
                    Printf.sprintf "Invalid CBA logging level: %s" level_name)
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
    option_metavars = ["CBA_LEVEL"]
    ;
    option_defhelp = Some("Selects a CBA logging level (none,result,all).")
    ;       
  }
;;
