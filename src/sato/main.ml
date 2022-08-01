open Core
open Sato_args
open Odefa_ast
open Odefa_ast.Ast
open Odefa_natural.On_to_odefa_maps

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
  let (program, on_to_odefa_maps, _) = 
    File_utils.read_source_sato dbmc_config_init.filename 
  in
  let init_sato_state = 
    Sato_state.initialize_state_with_expr program on_to_odefa_maps 
  in
  let target_vars = init_sato_state.target_vars in
  let rec search_all_targets 
    (remaining_targets : ident list) 
    : Sato_result.Type_errors.t =
    match remaining_targets with
    | [] -> failwith "???"
      (* print_endline "No errors found." *)
    | hd :: tl ->
      let dbmc_config = 
        { dbmc_config_init with target = hd }
      in
      (* Right now we're stopping after one error is found. *)
      (try
        let open Dbmc in
        let (inputss, _, dbmc_state) = 
          Dbmc.Main.main_details ~config:dbmc_config program 
        in
        match List.hd inputss with
        | Some inputs ->
          begin
            let session = 
              { Interpreter.default_session with input_feeder = 
                Input_feeder.from_list inputs 
              }
            in
            try
              (
              Interpreter.eval_verbose session program
              )
            with
            | Interpreter.Terminate_with_env (_, ab_clo) ->
              match ab_clo with
              | AbortClosure final_env ->
                let sato_res = 
                  Sato_result.Type_errors.get_result 
                    init_sato_state dbmc_state session final_env inputs
                in
                sato_res
              | _ -> failwith "Shoud have run into abort here!"
          end
        | None -> search_all_targets tl
      with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                  raise ex)
  in
  let errors = search_all_targets target_vars in
  print_endline @@ Sato_result.Type_errors.show errors;
  Dbmc.Log.close ()
