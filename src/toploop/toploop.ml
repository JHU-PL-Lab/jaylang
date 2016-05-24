open Batteries;;

open Ast;;
open Ast_pp;;
open Ast_wellformedness;;
open Ddpa_graph;;
open Interpreter;;
open Toploop_options;;

let logger = Logger_utils.make_logger "Toploop";;
let lazy_logger = Logger_utils.make_lazy_logger "Toploop";;

exception Invalid_variable_analysis of string;;

type toploop_configuration =
  { topconf_context_stack : (module Analysis_context_stack.Context_stack) option
  ; topconf_log_prefix : string
  ; topconf_ddpa_log_level : Ddpa_graph_logger.ddpa_graph_logger_level option
  ; topconf_pdr_log_level :
      Pds_reachability_logger_utils.pds_reachability_logger_level option
  ; topconf_analyze_vars : analyze_variables_selection
  ; topconf_disable_evaluation : bool
  ; topconf_disable_inconsistency_check : bool
  ; topconf_disable_analysis : bool
  ; topconf_report_sizes : bool
  }
;;

let toploop_operate conf e =
  print_string "\n";
  begin
    try
      check_wellformed_expr e;
      let context_stack_opt = conf.topconf_context_stack in
      let toploop_action evaluation_step =
        match context_stack_opt with
        | None ->
          evaluation_step ()
        | Some context_stack ->
          if conf.topconf_disable_analysis then
            evaluation_step ()
          else
            let module Context_stack = (val context_stack) in
            (* Define the analysis module. *)
            let module A = Analysis.Make(Context_stack) in
            (* Use the toploop wrapper on it. *)
            let module TLA = Toploop_ddpa.Make(A) in
            (* Set logging configuration. *)
            begin
              match conf.topconf_ddpa_log_level with
              | Some level -> Ddpa_graph_logger.set_level level
              | None -> ();
            end;
            begin
              match conf.topconf_pdr_log_level with
              | Some level -> A.set_pdr_logger_level level
              | None -> ();
            end;
            (* Create the analysis.  The wrapper performs full closure on it. *)
            let analysis =
              TLA.create_analysis ~logging_prefix: (Some "_toploop") e
            in
            (* Determine if it is consistent. *)
            let inconsistencies =
              if conf.topconf_disable_inconsistency_check
              then Enum.empty ()
              else TLA.check_inconsistencies analysis
            in
            (* If there are inconsistencies, report them. *)
            if not @@ Enum.is_empty inconsistencies
            then
              inconsistencies
              |> Enum.iter
                (fun inconsistency ->
                   print_endline @@ Toploop_ddpa.pp_inconsistency inconsistency)
            else
              begin
                (* Analyze the variables that the user requested.  If the user
                   requested non-existent variables, they are ignored. *)
                let analyses_desc =
                  match conf.topconf_analyze_vars with
                  | Analyze_no_variables -> []
                  | Analyze_toplevel_variables ->
                    e
                    |> (fun (Expr(cls)) -> cls)
                    |> List.enum
                    |> Enum.map lift_clause
                    |> Enum.map (fun (Abs_clause(Var(i,_), _)) -> (i, None, None))
                    |> List.of_enum
                  | Analyze_specific_variables lst ->
                    lst |> List.map (fun (x,y,z) -> (Ident(x),y,z))
                in
                (* We'll need a mapping from variable names to clauses. *)
                let varname_to_clause_map =
                  e
                  |> Ast_tools.find_all_clauses_in_expr
                  |> Enum.map lift_clause
                  |> Enum.map
                    (fun (Abs_clause(Var(i,_),_) as c) -> (i, c))
                  |> Ident_map.of_enum
                in
                let lookup_clause_by_ident ident =
                  try
                    Ident_map.find ident varname_to_clause_map
                  with
                  | Not_found -> raise @@
                    Invalid_variable_analysis(Printf.sprintf
                                                "No such variable: %s" (pp_ident ident))
                in
                (* Determine the analyses to perform. *)
                let analyses =
                  analyses_desc
                  |> List.enum
                  |> Enum.map
                    (fun (var_ident,site_name_opt,context_opt) ->
                       let site =
                         match site_name_opt with
                         | None -> End_clause
                         | Some site_name ->
                           Unannotated_clause(
                             lookup_clause_by_ident (Ident site_name))
                       in
                       let context_stack =
                         match context_opt with
                         | None -> TLA.C.empty
                         | Some context_vars ->
                           context_vars
                           |> List.enum
                           |> Enum.fold
                             (fun a e ->
                                let c = lookup_clause_by_ident (Ident e) in
                                TLA.C.push c a
                             )
                             TLA.C.empty
                       in
                       (Var(var_ident,None),site,context_stack)
                    )
                in
                (* Perform and render the analyses. *)
                if not @@ Enum.is_empty analyses then
                  begin
                    print_endline "Variable analyses:";
                    analyses
                    |> Enum.iter
                      (fun (var, site, context) ->
                         let values =
                           TLA.contextual_values_of_variable_from
                             var site context analysis
                         in
                         print_endline @@ Printf.sprintf
                           "    %s: %s"
                           (pp_var var) (pp_abs_filtered_value_set values)
                      );
                    print_endline "End variable analyses.";
                  end;
                (* Dump the analysis to debugging. *)
                lazy_logger `trace
                  (fun () -> Printf.sprintf "DDPA analysis: %s"
                      (TLA.pp_analysis analysis));
                if conf.topconf_report_sizes then
                  begin
                    let ddpa_number_of_active_nodes,
                        ddpa_number_of_active_non_immediate_nodes,
                        ddpa_number_of_edges,
                        pds_number_of_nodes,
                        pds_number_of_edges
                      = TLA.get_size analysis in
                    Printf.printf "DDPA number of active nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of active non immediate nodes (excluding enter and exit nodes that can be inferred): %n.\nDDPA number of edges: %n.\nPDS number of nodes: %n.\nPDS number of edges: %n.\n"
                      ddpa_number_of_active_nodes
                      ddpa_number_of_active_non_immediate_nodes
                      ddpa_number_of_edges
                      pds_number_of_nodes
                      pds_number_of_edges
                  end;
                (* Now run the actual program. *)
                evaluation_step ()
              end
      in
      toploop_action (fun () ->
          if conf.topconf_disable_evaluation
          then print_string "Evaluation disabled"
          else
            begin
              let v, env = eval e in
              print_string (pp_var v ^ " where " ^ pp_env env ^ "\n")
            end
        )
    with
    | Illformedness_found(ills) ->
      print_string "Provided expression is ill-formed:\n";
      List.iter
        (fun ill ->
           print_string @@ "   " ^ pp_illformedness ill ^ "\n")
        ills
  end;
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout
;;

let command_line_parsing () =
  let parser = BatOptParse.OptParser.make ~version:"version 0.3" () in

  (* Add logging options *)
  BatOptParse.OptParser.add parser ~long_name:"log" logging_option;

  (* Add ability to select the context stack. *)
  BatOptParse.OptParser.add parser ~long_name:"select-context-stack"
    ~short_name:'S' select_context_stack_option;

  (* Add DDPA graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"ddpa-logging"
    ddpa_logging_option;

  (* Add PDS reachability graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"pdr-logging" pdr_logging_option;

  (* Add control over variables used in toploop analysis. *)
  BatOptParse.OptParser.add parser ~long_name:"analyze-variables"
    analyze_variables_option;

  (* Add control over whether evaluation actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-evaluation"
    ~short_name:'E' disable_evaluation_option;

  (* Add control over whether evaluation actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-inconsistency-check"
    ~short_name:'I' disable_inconsistency_check_option;

  (* Add control over whether analysis actually occurs. *)
  BatOptParse.OptParser.add parser ~long_name:"disable-analisys"
    ~short_name:'A' disable_analysis_option;

  (* Add ability to report sizes of generated graphs. *)
  BatOptParse.OptParser.add parser ~long_name:"report-sizes"
    report_sizes_option;

  (* Handle arguments. *)
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] ->
    { topconf_context_stack =
      Option.get @@ select_context_stack_option.BatOptParse.Opt.option_get ()
    ; topconf_log_prefix = "_toploop"
    ; topconf_ddpa_log_level = ddpa_logging_option.BatOptParse.Opt.option_get ()
    ; topconf_pdr_log_level = pdr_logging_option.BatOptParse.Opt.option_get ()
    ; topconf_analyze_vars = Option.get @@
        analyze_variables_option.BatOptParse.Opt.option_get ()
    ; topconf_disable_evaluation = Option.get @@
        disable_evaluation_option.BatOptParse.Opt.option_get ()
    ; topconf_disable_inconsistency_check = Option.get @@
        disable_inconsistency_check_option.BatOptParse.Opt.option_get ()
    ; topconf_disable_analysis = Option.get @@
        disable_analysis_option.BatOptParse.Opt.option_get ()
    ; topconf_report_sizes = Option.get @@
        report_sizes_option.BatOptParse.Opt.option_get ()
    }
  | _ -> failwith "Unexpected command-line arguments."
;;

let () =
  let toploop_configuration = command_line_parsing () in

  print_string "Toy Toploop\n";
  print_string "-----------\n";
  print_string "\n";
  print_string "Please enter an expression to evaluate followed by \";;\".\n";
  print_string "\n";
  flush stdout;
  Parser.parse_expressions IO.stdin
  |> LazyList.iter (toploop_operate toploop_configuration)
;;
