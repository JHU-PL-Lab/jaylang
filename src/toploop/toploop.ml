open Batteries;;

open Ast;;
open Ast_pretty;;
open Ast_wellformedness;;
open Cba_graph;;
open Interpreter;;
open Toploop_options;;

let logger = Logger_utils.make_logger "Toploop";;

type toploop_configuration =
  { topconf_context_stack : (module Analysis_context_stack.Context_stack) option
  ; topconf_log_prefix : string
  ; topconf_cba_log_level : Cba_graph_logger.cba_graph_logger_level option
  }
;;

let rec find_all_call_sites (Expr cls) =
  let rec call_sites_from_function_value (Function_value(_,e)) =
    find_all_call_sites e
  in
  let rec call_sites_from_clause cl =
    match cl with
    | Clause(_,Appl_body(x2,_)) ->
      Some (Enum.singleton (x2,cl))
    | Clause(_,Value_body(Value_function(f))) ->
      Some (call_sites_from_function_value f)
    | Clause(_,Conditional_body(_,_,f1,f2)) ->
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
          begin
            match conf.topconf_cba_log_level with
            | Some level -> Cba_graph_logger.set_level level
            | None -> ();
          end;
          (* Create the initial analysis. *)
          let a1 =
            A.create_initial_analysis ~logging_prefix:(Some "_toploop") e
          in
          (* Close over the analysis. *)
          let a2 = A.perform_full_closure a1 in
          (* Check the consistency of an analysis.  In particular, look for
             call sites where non-function values appear. *)
          let aref = ref a2 in
          let inconsistencies =
            find_all_call_sites e
            |> Enum.map
              (fun (x2,cl) ->
                let acl = Unannotated_clause(lift_clause cl) in
                let (values,a') = A.values_of_variable acl x2 !aref in
                aref := a';
                values
                |> Abs_value_set.enum
                |> Enum.filter_map
                  (fun v ->
                    match v with
                    | Abs_value_function _ -> None
                    | _ -> Some (x2,cl,v)
                  )
              )
            |> Enum.concat
            |> List.of_enum (* force us to pull on the enum so the analysis
                               updates *)
          in
          let a3 = !aref in
          (* If there are inconsistencies, report them. *)
          if not @@ List.is_empty inconsistencies
          then
            raise @@ Utils.Not_yet_implemented "toploop_operate"
          else
            begin
              (* Show the value of each top-level clause in the program. *)
              let acls =
                e
                |> (fun (Expr(cls)) -> cls)
                |> List.enum
                |> Enum.map lift_clause
                |> Enum.map (fun x -> Unannotated_clause(x))
                |> flip Enum.append (Enum.singleton End_clause)
              in
              let acls' = Enum.clone acls in
              ignore @@ Enum.get_exn acls';
              let acl_pairs = Enum.combine (acls,acls') in
              let (variable_values, a4) = 
                acl_pairs
                |> Enum.fold
                  (fun (m,a) (acl1,acl0) ->
                    match acl1 with
                    | Unannotated_clause(Abs_clause(x,_)) ->
                      let (vs,a') = (A.values_of_variable acl0 x a) in
                      let Var(i,_) = x in
                      let m' = Ident_map.add i vs m in
                      (m',a')
                    | _ -> (m,a)
                  ) (Ident_map.empty, a3)
              in
              (* Show our results. *)
              print_endline @@
                pretty_ident_map pp_abs_value_set variable_values;
              (* Shush the warning about a4. *)
              logger `trace (Printf.sprintf "CBA analysis: %s" (A.pp_cba a4));
              ignore a4;
              (* Now run the actual program. *)
              evaluation_step ()
            end
      in
      toploop_action (fun () ->
          let v,env = eval e in
          print_string (pretty_var v ^ " where "  ^ pretty_env env ^ "\n")
        )
    with
    | Illformedness_found(ills) ->
      print_string "Provided expression is ill-formed:\n";
      List.iter
        (fun ill ->
           print_string @@ "   " ^ pretty_illformedness ill ^ "\n")
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
    
  (* Add CBA graph logging option. *)
  BatOptParse.OptParser.add parser ~long_name:"cba-logging" cba_logging_option;
  
  (* Handle arguments. *)
  let spare_args = BatOptParse.OptParser.parse_argv parser in
  match spare_args with
  | [] ->
    { topconf_context_stack =
      Option.get @@ select_context_stack_option.BatOptParse.Opt.option_get ()
    ; topconf_log_prefix = "_toploop"
    ; topconf_cba_log_level = cba_logging_option.BatOptParse.Opt.option_get ()
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
