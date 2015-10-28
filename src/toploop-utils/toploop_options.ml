open Batteries;;
open BatOptParse.Opt;;

open Analysis_instances;;
open Cba_graph_logger;;
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

module type A = Analysis.Analysis_sig;;

let select_analysis_option =
  (* This ref contains a module option.  If the option is None, no analysis is
     to be performed. *)
  let analysis_module_ref = ref (Some (module Cba_1stack : A)) in
  {
    option_set = (fun option_name args ->
      match args with
      | [analysis_name] ->
        let analysis_module =
          begin
            match analysis_name with
            | "cba0" -> Some (module Cba_0stack : A)
            | "cba1" -> Some (module Cba_1stack : A)
            | "cba2" -> Some (module Cba_2stack : A)
            | "cbanr" -> Some (module Cba_nonrepeating_stack : A)
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
  (* No closure context is needed here, since we're configuring a global
     module. *)
  {
    option_set = (fun option_name args ->
      match args with
      | [level_name] ->
        let level =
          match level_name with
          | "none" -> Cba_log_none
          | "result" -> Cba_log_result
          | "all" -> Cba_log_all
          | _ -> raise @@ Option_error(option_name,
                    Printf.sprintf "Invalid CBA logging level: %s" level_name)
        in
        Cba_graph_logger.set_level level
      | _ ->
        raise @@ Option_error (option_name,
          Printf.sprintf "Invalid argument count: %d" (List.length args))
      )
    ;
    option_set_value = (fun level -> Cba_graph_logger.set_level level)
    ;
    option_get = (fun () -> Some (Cba_graph_logger.get_level ()))
    ;
    option_metavars = ["CBA_LEVEL"]
    ;
    option_defhelp = Some("Selects a CBA logging level (none,result,all).")
    ;       
  }
;;
