open Batteries;;

open Ast;;
open Ast_pp;;
open Ast_wellformedness;;
open Ddpa_graph;;
open Interpreter;;
open Toploop_options;;

let logger = Logger_utils.make_logger "Toploop";;

type toploop_configuration =
  { topconf_context_stack : (module Analysis_context_stack.Context_stack) option
  ; topconf_log_prefix : string
  ; topconf_ddpa_log_level : Ddpa_graph_logger.ddpa_graph_logger_level option
  ; topconf_pdr_log_level :
      Pds_reachability_logger_utils.pds_reachability_logger_level option
  ; topconf_analyze_vars : (ident -> bool)
  }
;;

(** Finds all of the call sites in the provided expression.  Returns an
    enumeration of call sites as pairs between the function variable and the
    clause representing the call site. *)
let rec find_all_call_sites (Expr cls) =
  let rec call_sites_from_function_value (Function_value(_, e)) =
    find_all_call_sites e
  in
  let rec call_sites_from_clause cl =
    match cl with
    | Clause(_, Appl_body(x2, _)) ->
      Some (Enum.singleton (x2, cl))
    | Clause(_, Value_body(Value_function(f))) ->
      Some (call_sites_from_function_value f)
    | Clause(_, Conditional_body(_, _, f1, f2)) ->
      Some (Enum.append (call_sites_from_function_value f1)
                        (call_sites_from_function_value f2))
    | _ -> None
  in
  cls
  |> List.enum
  |> Enum.filter_map
    call_sites_from_clause
  |> Enum.concat
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
          let inconsistencies = TLA.check_inconsistencies analysis in
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
              let vars_to_analyze =
                e
                |> (fun (Expr(cls)) -> cls)
                |> List.enum
                |> Enum.map lift_clause
                |> Enum.map (fun (Abs_clause(x, _)) -> x)
                |> Enum.filter
                  (fun (Var(i,_)) -> conf.topconf_analyze_vars i)
              in
              if not @@ Enum.is_empty vars_to_analyze then
              begin
                let variable_values =
                  vars_to_analyze
                  |> Enum.fold
                    (fun m x ->
                      let vs =
                        TLA.values_of_variable_from x End_clause analysis
                      in
                      let (Var(i,_)) = x in
                      Ident_map.add i vs m
                    ) Ident_map.empty
                in
                (* Show our results. *)
                print_endline @@
                  pp_ident_map pp_abs_filtered_value_set variable_values
              end;
              (* Dump the analysis to debugging. *)
              logger `trace
                (Printf.sprintf "DDPA analysis: %s" (TLA.pp_analysis analysis));
              (* Now run the actual program. *)
              evaluation_step ()
            end
      in
      toploop_action (fun () ->
          let v, env = eval e in
          print_string (pp_var v ^ " where " ^ pp_env env ^ "\n")
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
