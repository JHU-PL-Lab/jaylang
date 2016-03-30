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

type analyze_variables_selection =
  | Analyze_no_variables
  | Analyze_toplevel_variables
  | Analyze_specific_variables of
      (string * string option * string list option) list
;;

let analyze_variables_option =
  let variables_to_analyze = ref Analyze_no_variables in
  {
    option_set = (fun option_name args ->
      match args with
      | [analyze_string] ->
        let new_selection =
          match analyze_string with
          | "none" -> Analyze_no_variables
          | "all" -> Analyze_toplevel_variables
          | _ ->
            if String.starts_with analyze_string "only:"
            then
              let components =
                analyze_string
                |> String.lchop ~n:5
                |> (fun x -> String.nsplit x ~by:",")
              in
              let parse_component component =
                begin
                  match String.nsplit component ~by:"@" with
                  | [name] -> (name, None, None)
                  | [name;rest] ->
                    begin
                      match String.nsplit rest ~by:":" with
                      | [loc] -> (name, Some loc, None)
                      | [loc;stack] ->
                        begin
                          let stack_elements = String.nsplit stack ~by:"|" in
                          (name, Some loc, Some stack_elements)
                        end
                      | _ -> raise @@ Option_error (option_name,
                                Printf.sprintf "Invalid component string: %s"
                                  component)
                    end
                  | _ -> raise @@ Option_error (option_name,
                            Printf.sprintf "Invalid component string: %s"
                              component)
                end
              in
              Analyze_specific_variables(List.map parse_component components)
            else
              raise @@ Option_error (option_name,
                Printf.sprintf "Unrecognized variable analysis mode: %s"
                  analyze_string)
        in
        variables_to_analyze := new_selection
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun selection -> variables_to_analyze := selection)
    ;
    option_get = (fun () -> Some (!variables_to_analyze))
    ;
    option_metavars = ["ANALYZE_SPEC"]
    ;
    option_defhelp =
      Some("Selects variables to analyze.  Valid options are \"none\" (to \
            perform no variable-specific analysis), \"top\" (to analyze all \
            top-level variables from the end of the program), or \"only\".  \
            If \"only\" is given, it must be followed by a colon and then a \
            comma-separated list of specifications.  A specification is the \
            name of a variable (e.g. \"a\"), optionally followed by \"@\" and \
            a lookup site variable (e.g. \"s\"), optionally followed by \":\" \
            and a pipe-separated list of variable names representing a context \
            stack (from top to bottom).")
    ;
  }
;;

let disable_evaluation_option =
  BatOptParse.StdOpt.store_true ()
;;
  
let disable_inconsistency_check_option =
  BatOptParse.StdOpt.store_true ()
;;

let disable_analysis_option =
  BatOptParse.StdOpt.store_true ()
;;

let report_sizes_option =
  BatOptParse.StdOpt.store_true ()
;;
