open Core
open Sato_args
open Odefa_ast.Ast

(* Enumerate all aborts in a program *)

let rec enum_all_aborts_in_expr expr : (ident * abort_value) list =
  let Expr clauses = expr in
  List.concat @@ List.map ~f:enum_all_aborts_in_clause clauses 

and enum_all_aborts_in_clause clause : (ident * abort_value) list =
  let Clause (Var (cls_id, _), body) = clause in
  match body with
  | Conditional_body (Var (pred_id, _), e1, e2) ->
    begin
      let enum_ret_abort e branch =
        let Expr(c_list) = e in
        match List.last c_list with
        | None -> []
        | Some cls ->
          begin
            match cls with
            | Clause (Var (abort_id, _), Abort_body) ->
              let abort_val = {
                abort_conditional_ident = cls_id;
                abort_predicate_ident = pred_id;
                abort_conditional_branch = branch;
              }
              in
              [(abort_id, abort_val)]
            | _ -> []
          end
      in
      []
      |> List.append (enum_all_aborts_in_expr e1)
      |> List.append (enum_ret_abort e1 true)
      |> List.append (enum_all_aborts_in_expr e2)
      |> List.append (enum_ret_abort e2 false)
    end
  | Value_body v ->
    enum_all_aborts_in_value v
  | Abort_body (* Aborts are enumerated in conditionals *)
  | Assume_body _
  | Assert_body _
  | Var_body _
  | Input_body
  | Appl_body (_, _)
  | Binary_operation_body (_, _, _)
  | Not_body _
  | Match_body (_, _)
  | Projection_body (_, _) ->
    []

and enum_all_aborts_in_value value : (ident * abort_value) list =
  match value with
  | Value_function (Function_value (_, e)) ->
    enum_all_aborts_in_expr e
  | Value_int _ | Value_bool _ | Value_record _ ->
    []
;;

let list_instrument_conditionals (e : expr) : ident list =
  e
  |> enum_all_aborts_in_expr
  (* |> List.map ~f:(fun (_, abort_val) -> abort_val.abort_conditional_ident) *)
  |> List.map ~f:(fun (abort_id, _) -> abort_id)
  |> List.rev
;;

let get_target_vars (expr : Odefa_ast.Ast.expr) : ident list =
  match list_instrument_conditionals expr with
  | [] -> failwith "TBI!"
  | target_list -> target_list
;;

let create_initial_dmbc_config (sato_config : Sato_args.t) 
  : Dbmc.Global_config.t =
  (* Extract basic configuration from sato args *)
  let filename = sato_config.filename in
  let ddpa_ver = sato_config.ddpa_c_stk in
  let max_step = sato_config.run_max_step in
  let timeout = sato_config.timeout in
  let open Dbmc.Global_config in
  {
    target = Dbmc.Id.(Ident "target");
    filename = filename;
    engine = E_dbmc;
    is_instrumented = false;
    mode = Sato;
    ddpa_c_stk = ddpa_ver;
    run_max_step = max_step;
    timeout = timeout;
    stride_init = 100;
    stride_max = 100;
    log_level = None;
    log_level_lookup = None;
    log_level_solver = None;
    log_level_interpreter = None;
    debug_phi = false;
    debug_model = true;
    debug_graph = false;
  }
;;

let main_commandline () =
  let sato_config = Argparse.parse_commandline_config () in
  let dbmc_config_init = create_initial_dmbc_config sato_config in
  let (program, _, _) = File_util.read_source_sato dbmc_config_init.filename in
  let target_vars = get_target_vars program in
  let rec search_all_targets (remaining_targets : ident list) =
    match remaining_targets with
    | [] -> print_endline "No errors found."
    | hd :: tl ->
      let dbmc_config = 
        { dbmc_config_init with target = hd }
      in
      (* Right now we're stopping after one error is found. *)
      (try
        let (inputss, _, _) = Dbmc.Main.main_details ~config:dbmc_config program in
        match List.hd inputss with
        | Some inputs ->
            Format.printf "[%s]\n"
              (String.concat ~sep:","
              @@ List.map
                    ~f:(function Some i -> string_of_int i | None -> "-")
                    inputs)
        | None -> search_all_targets tl
      with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                  raise ex)
  in
  search_all_targets target_vars;
  Dbmc.Log.close ()
