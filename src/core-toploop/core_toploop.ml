open Batteries;;

open Core_ast;;
open Core_ast_pp;;
open Core_ast_wellformedness;;
open Core_interpreter;;
open Core_toploop_option_parsers;;
open Core_toploop_options;;
open Core_toploop_types;;
open Ddpa_graph;;

exception Invalid_variable_analysis of string;;

let stdout_illformednesses_callback ills =
  print_string "Provided expression is ill-formed:\n";
  List.iter
    (fun ill -> print_string @@ "   " ^ show_illformedness ill ^ "\n")
    ills;
  flush stdout
;;

let stdout_variable_analysis_callback
    var_name site_name_opt context_opt values =
  print_string "Lookup of variable ";
  print_string var_name;
  begin
    match site_name_opt with
    | Some site_name ->
      print_string " from clause ";
      print_string site_name;
    | None -> ()
  end;
  begin
    match context_opt with
    | Some context ->
      print_string " in context ";
      let rec loop ss =
        match ss with
        | [] -> print_string "[]"
        | s::[] -> print_string s
        | s::ss' ->
          print_string s;
          print_string "|";
          loop ss'
      in
      loop context
    | None -> ()
  end;
  print_endline " yields values:";
  print_string "    ";
  print_string @@ show_abs_filtered_value_set values;
  flush stdout
;;

let stdout_errors_callback errors =
  errors
  |> List.iter
    (fun error ->
       print_string @@ Core_toploop_analysis_types.show_error error
    );
  flush stdout
;;

let stdout_evaluation_result_callback v env =
  print_endline (show_var v ^ " where " ^ show_evaluation_environment env ^ "\n");
  flush stdout
;;

let stdout_evaluation_failed_callback msg =
  print_endline @@ "Evaluation failed: " ^ msg;
  flush stdout
;;

let stdout_evaluation_disabled_callback () =
  print_endline "Evaluation disabled";
  flush stdout
;;

let stdout_size_report_callback
    ( ddpa_number_of_active_nodes
    , ddpa_number_of_active_non_immediate_nodes
    , ddpa_number_of_edges
    , pds_number_of_nodes
    , pds_number_of_edges
    ) =
  Printf.printf "DDPA number of active nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of active non immediate nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of edges: %n.\nPDS number of nodes: %n.\nPDS number of edges: %n.\n"
    ddpa_number_of_active_nodes
    ddpa_number_of_active_non_immediate_nodes
    ddpa_number_of_edges
    pds_number_of_nodes
    pds_number_of_edges;
  flush stdout
;;

let no_op_callbacks =
  { cb_illformednesses = (fun _ -> ())
  ; cb_variable_analysis = (fun _ _ _ _ -> ())
  ; cb_errors = (fun _ -> ())
  ; cb_evaluation_result = (fun _ _ -> ())
  ; cb_evaluation_failed = (fun _ -> ())
  ; cb_evaluation_disabled = (fun _ -> ())
  ; cb_size_report_callback = (fun _ -> ())
  }
;;

let stdout_callbacks =
  { cb_illformednesses = stdout_illformednesses_callback
  ; cb_variable_analysis = stdout_variable_analysis_callback
  ; cb_errors = stdout_errors_callback
  ; cb_evaluation_result = stdout_evaluation_result_callback
  ; cb_evaluation_failed = stdout_evaluation_failed_callback
  ; cb_evaluation_disabled = stdout_evaluation_disabled_callback
  ; cb_size_report_callback = stdout_size_report_callback
  }
;;

let do_analysis_steps callbacks conf e =
  (* If no one wants an analysis, don't waste the effort. *)
  if conf.topconf_disable_inconsistency_check &&
     conf.topconf_analyze_vars ==
     Analyze_no_variables &&
     not conf.topconf_report_sizes
  then ([], [])
  else
    match conf.topconf_context_stack with
    | None -> ([], []) (* Nothing can be done without a context stack. *)
    | Some context_stack ->
      (* We're finally ready to perform some analyses.  Unpack the context
         stack. *)
      let module Context_stack = (val context_stack) in
      (* Define the analysis module. *)
      let module Analysis = Ddpa_analysis.Make(Context_stack) in
      (* Define the convenience wrapper. *)
      let module DDPA_wrapper = Core_toploop_ddpa_wrapper.Make(Analysis) in
      (* Set up the logging configuration for the analysis. *)
      begin
        match conf.topconf_ddpa_log_level with
        | Some level -> Ddpa_graph_logger.set_level level
        | None -> ();
      end;
      begin
        match conf.topconf_pdr_log_level with
        | Some level -> Analysis.set_pdr_logger_level level
        | None -> ();
      end;
      (* Create the analysis.  The wrapper performs full closure on it. *)
      let analysis = DDPA_wrapper.create_analysis e in
      (* We'll now define a couple of functions to perform the analysis-related
         tasks and then call them below. *)
      (* This function performs a simple error check. *)
      let check_for_errors () =
        if conf.topconf_disable_inconsistency_check
        then []
        else
          let module Error_analysis =
            Core_toploop_analysis.Make(DDPA_wrapper)
          in
          let errors = List.of_enum @@ Error_analysis.find_errors analysis in
          callbacks.cb_errors errors;
          errors
      in
      (* This function takes the configuration option describing the variable
         analysis requested on the command line and standardizes the form of
         the request. *)
      let standardize_variable_analysis_request () =
        match conf.topconf_analyze_vars with
        | Analyze_no_variables -> None
        | Analyze_toplevel_variables ->
          Some(
            e
            |> (fun (Expr(cls)) -> cls)
            |> List.enum
            |> Enum.map lift_clause
            |> Enum.map (fun (Abs_clause(Var(Ident i,_), _)) -> (i, None, None))
            |> List.of_enum
          )
        | Analyze_specific_variables lst -> Some lst
      in
      (* Given a set of variable analysis requests, this function performs
         them. *)
      let analyze_variable_values requests =
        (* We'll need a mapping from variable names to clauses. *)
        let varname_to_clause_map =
          e
          |> Core_ast_tools.flatten
          |> List.map lift_clause
          |> List.map
            (fun (Abs_clause(Var(i,_),_) as c) -> (i, c))
          |> List.enum
          |> Ident_map.of_enum
        in
        (* This utility function helps us use the mapping. *)
        let lookup_clause_by_ident ident =
          try
            Ident_map.find ident varname_to_clause_map
          with
          | Not_found -> raise @@
            Invalid_variable_analysis(
              Printf.sprintf "No such variable: %s" (show_ident ident))
        in
        (* Perform each of the requested analyses. *)
        requests
        |> List.enum
        |> Enum.map
          (fun (var_name,site_name_opt,context_opt) ->
             let var_ident = Ident var_name in
             let lookup_var = Var(var_ident,None) in
             let site =
               match site_name_opt with
               | None -> End_clause
               | Some site_name ->
                 Unannotated_clause(
                   lookup_clause_by_ident (Ident site_name))
             in
             let context_stack =
               match context_opt with
               | None -> DDPA_wrapper.C.empty
               | Some context_vars ->
                 context_vars
                 |> List.enum
                 |> Enum.fold
                   (fun a e ->
                      let c = lookup_clause_by_ident (Ident e) in
                      DDPA_wrapper.C.push c a
                   )
                   DDPA_wrapper.C.empty
             in
             let values =
               DDPA_wrapper.contextual_values_of_variable_from
                 lookup_var site context_stack analysis
             in
             callbacks.cb_variable_analysis
               var_name site_name_opt context_opt values;
             ((var_name,site_name_opt,context_opt),values)
          )
        |> List.of_enum
      in
      (* At this point, dump the analysis to debugging if appropriate. *)
      lazy_logger `trace
        (fun () -> Printf.sprintf "DDPA analysis: %s"
            (DDPA_wrapper.show_analysis analysis));
      (* If size reporting has been requested, do that too. *)
      if conf.topconf_report_sizes
      then callbacks.cb_size_report_callback @@ DDPA_wrapper.get_size analysis;
      (* Now we'll call the above routines. *)
      let errors = check_for_errors () in
      let analyses =
        match standardize_variable_analysis_request () with
        | None -> []
        | Some requests -> analyze_variable_values requests
      in
      (analyses, errors)
;;

let do_evaluation callbacks conf e =
  if conf.topconf_disable_evaluation
  then
    begin
      callbacks.cb_evaluation_disabled ();
      Core_toploop_types.Evaluation_disabled
    end
  else
    begin
      try
        let v, env = Core_interpreter.eval e in
        callbacks.cb_evaluation_result v env;
        Core_toploop_types.Evaluation_completed(v,env)
      with
      | Core_interpreter.Evaluation_failure s ->
        Core_toploop_types.Evaluation_failure s
    end
;;

let handle_expression
    ?callbacks:(callbacks=no_op_callbacks)
    conf
    e =
  try
    (* Step 1: check for inconsistencies! *)
    check_wellformed_expr e;
    (* Step 2: perform analyses.  This covers both variable analyses and
       error checking. *)
    let analyses, errors = do_analysis_steps callbacks conf e in
    (* Step 3: perform evaluation. *)
    let evaluation_result =
      if errors = []
      then do_evaluation callbacks conf e
      else Evaluation_invalidated
    in
    (* Generate answer. *)
    { illformednesses = []
    ; analyses = analyses
    ; errors = errors
    ; evaluation_result = evaluation_result
    }
  with
  | Illformedness_found(ills) ->
    callbacks.cb_illformednesses ills;
    { illformednesses = ills
    ; analyses = []
    ; errors = []
    ; evaluation_result = Evaluation_invalidated
    }
;;
