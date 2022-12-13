open Core
open Jayil.Ast
open Jay_translate.Jay_to_jayil_maps
open Sato_args
open Sato_result

let create_initial_dmbc_config (sato_config : Sato_args.t) :
    Dj_common.Global_config.t =
  (* Extract basic configuration from sato args *)
  let open Dj_common in
  let filename = sato_config.filename in
  let ddpa_ver = sato_config.ddpa_c_stk in
  let max_step = sato_config.run_max_step in
  let timeout = sato_config.timeout in
  let open Dj_common.Global_config in
  {
    Global_config.default_config with
    filename;
    engine = E_dbmc;
    is_instrumented = false;
    mode = Sato;
    ddpa_c_stk = ddpa_ver;
    run_max_step = max_step;
    timeout;
  }

let main_from_program ~config inst_maps odefa_to_on_opt ton_to_on_opt program :
    reported_error option * bool =
  let dbmc_config_init = create_initial_dmbc_config config in
  Dj_common.Log.init dbmc_config_init ;
  let sato_mode = config.sato_mode in
  let init_sato_state =
    Sato_state.initialize_state_with_expr sato_mode program inst_maps
      odefa_to_on_opt ton_to_on_opt
  in
  let target_vars = init_sato_state.target_vars in
  let rec search_all_targets (remaining_targets : ident list)
      (has_timeout : bool) : reported_error option * bool =
    match remaining_targets with
    | [] -> (None, has_timeout)
    | hd :: tl -> (
        let () = print_endline "Lookup target: " in
        let () = print_endline @@ show_ident hd in
        let dbmc_config = { dbmc_config_init with target = hd } in
        (* Right now we're stopping after one error is found. *)
        try
          let open Dbmc in
          let Main.{ inputss; state = dbmc_state; is_timeout; _ } =
            Dbmc.Main.main ~config:dbmc_config program
          in
          match List.hd inputss with
          | Some inputs -> (
              let history = ref inputs in
              let session =
                {
                  (Interpreter.make_default_session ()) with
                  input_feeder = Input_feeder.from_list inputs ~history;
                }
              in
              try Interpreter.eval session program
              with Interpreter.Found_abort ab_clo -> (
                match ab_clo with
                | AbortClosure final_env -> (
                    match sato_mode with
                    | Bluejay ->
                        let errors =
                          Sato_result.Bluejay_type_errors.get_errors
                            init_sato_state dbmc_state session final_env inputs
                        in
                        (Some (Bluejay_error errors), has_timeout)
                    | Jay ->
                        let errors =
                          Sato_result.Jay_type_errors.get_errors init_sato_state
                            dbmc_state session final_env inputs
                        in
                        (Some (Jay_error errors), has_timeout)
                    | Jayil ->
                        let errors =
                          Sato_result.Jayil_type_errors.get_errors
                            init_sato_state dbmc_state session final_env inputs
                        in
                        (Some (Jayil_error errors), has_timeout))
                | _ -> failwith "Shoud have run into abort here!"))
          | None ->
              if is_timeout
              then search_all_targets tl true
              else search_all_targets tl has_timeout
        with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                   raise ex)
  in
  search_all_targets target_vars false

let main_commandline () =
  let sato_config = Argparse.parse_commandline_config () in
  let program, odefa_inst_maps, on_to_odefa_maps_opt, ton_to_on_mapts_opt =
    File_utils.read_source_sato ~do_wrap:sato_config.do_wrap
      ~do_instrument:sato_config.do_instrument sato_config.filename
  in
  let errors_res =
    main_from_program ~config:sato_config odefa_inst_maps on_to_odefa_maps_opt
      ton_to_on_mapts_opt program
  in
  let () =
    match errors_res with
    | None, false -> print_endline @@ "No errors found."
    | None, true ->
        print_endline
        @@ "Some search timed out; inconclusive result. Please run again with \
            longer timeout setting."
    | Some errors, _ -> print_endline @@ show_reported_error errors
  in
  Dj_common.Log.close ()
