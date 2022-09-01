open Core
open Odefa_ast.Ast
open Odefa_natural.On_to_odefa_maps
open Sato_args
open Sato_result

let create_initial_dmbc_config (sato_config : Sato_args.t) :
    Dbmc.Global_config.t =
  (* Extract basic configuration from sato args *)
  let filename = sato_config.filename in
  let ddpa_ver = sato_config.ddpa_c_stk in
  let max_step = sato_config.run_max_step in
  let timeout = sato_config.timeout in
  let open Dbmc.Global_config in
  {
    target = Dbmc.Id.(Ident "target");
    filename;
    engine = E_dbmc;
    is_instrumented = false;
    mode = Sato;
    ddpa_c_stk = ddpa_ver;
    run_max_step = max_step;
    timeout;
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

let main_from_program ~config inst_maps odefa_to_on_opt _ton_to_on_opt program :
    reported_error option =
  let dbmc_config_init = create_initial_dmbc_config config in
  let is_natodefa = config.is_natodefa in
  let init_sato_state =
    Sato_state.initialize_state_with_expr is_natodefa program inst_maps
      odefa_to_on_opt
  in
  let target_vars = init_sato_state.target_vars in
  let rec search_all_targets (remaining_targets : ident list) :
      reported_error option =
    match remaining_targets with
    | [] -> None
    | hd :: tl -> (
        let dbmc_config = { dbmc_config_init with target = hd } in
        (* Right now we're stopping after one error is found. *)
        try
          let open Dbmc in
          let inputss, _, dbmc_state =
            Dbmc.Main.main_details ~config:dbmc_config program
          in
          match List.hd inputss with
          | Some inputs -> (
              let session =
                {
                  (Interpreter.make_default_session ()) with
                  input_feeder = Input_feeder.from_list inputs;
                }
              in
              try Interpreter.eval session program
              with Interpreter.Found_abort ab_clo -> (
                match ab_clo with
                | AbortClosure final_env ->
                    if is_natodefa
                    then
                      let errors =
                        Sato_result.Natodefa_type_errors.get_errors
                          init_sato_state dbmc_state session final_env inputs
                      in
                      Some (Natodefa_error errors)
                    else
                      let errors =
                        Sato_result.Odefa_type_errors.get_errors init_sato_state
                          dbmc_state session final_env inputs
                      in
                      Some (Odefa_error errors)
                | _ -> failwith "Shoud have run into abort here!"))
          | None -> search_all_targets tl
        with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                   raise ex)
  in
  search_all_targets target_vars

let main_commandline () =
  let sato_config = Argparse.parse_commandline_config () in
  let program, odefa_inst_maps, on_to_odefa_maps_opt, _ =
    File_utils.read_source_sato sato_config.filename
  in
  let errors_opt =
    main_from_program ~config:sato_config odefa_inst_maps on_to_odefa_maps_opt
      None program
  in
  let () =
    match errors_opt with
    | None -> print_endline @@ "No errors found."
    | Some errors -> print_endline @@ show_reported_error errors
  in
  Dbmc.Log.close ()
