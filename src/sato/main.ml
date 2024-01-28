open Core
open Jayil.Ast
open Sato_result
open Dj_common

let dump_program program filename dump_level =
  match dump_level with
  | Global_config.Instrumented ->
      let program = Convert.jil_ast_of_convert program in
      let purged_expr = Jayil.Ast_tools.purge program in
      let og_file = Filename.chop_extension (Filename.basename filename) in
      let new_file = og_file ^ "_instrumented.jil" in
      File_utils.dump_jil_to_file purged_expr new_file
  | Global_config.All -> (
      let bluejay_program_opt = Convert.bluejay_ast_of_convert program in
      let jay_program_opt = Convert.jay_ast_of_convert program in
      match (bluejay_program_opt, jay_program_opt) with
      | Some bluejay_program, Some jay_program ->
          let jil_program = Convert.jil_ast_of_convert program in
          let purged_expr = Jayil.Ast_tools.purge jil_program in
          let og_file = Filename.chop_extension (Filename.basename filename) in
          let bluejay_file = og_file ^ "_og.bjy" in
          let jay_file = og_file ^ "_erased.jay" in
          let jil_file = og_file ^ "_instrumented.jil" in
          File_utils.dump_bluejay_to_file bluejay_program bluejay_file ;
          File_utils.dump_jay_to_file jay_program jay_file ;
          File_utils.dump_jil_to_file purged_expr jil_file
      | _ -> failwith "Should have bluejay and jay programs!")
  | _ -> ()

let main_lwt ~(config : Global_config.t) program_full :
    (reported_error option * bool) Lwt.t =
  dump_program program_full config.filename config.dump_level ;

  let program = Convert.jil_ast_of_convert program_full in
  (* let () = Fmt.pr "%a" Jayil.Pp.expr program in *)
  Log.init config ;
  let sato_mode =
    match config.mode with Sato m -> m | _ -> failwith "not sato"
  in
  let init_sato_state =
    Sato_state.initialize_state_with_expr sato_mode program_full
  in
  let target_vars = init_sato_state.target_vars in
  Fmt.pr "[SATO] #tgt=%d@.@?" (List.length target_vars) ;
  let rec search_all_targets (remaining_targets : ident list)
      (has_timeout : bool) : (reported_error option * bool) Lwt.t =
    match remaining_targets with
    | [] -> Lwt.return (None, has_timeout)
    | hd :: tl -> (
        let dbmc_config = { config with target = hd } in
        (* Right now we're stopping after one error is found. *)
        try
          let open Dbmc in
          Fmt.pr "[SATO] #tgt=%a  #s=%a@.@?" Ident.pp hd
            (Fmt.option Time_float.Span.pp)
            dbmc_config.timeout ;
          let state = Dbmc.Global_state.create dbmc_config program in
          let abort_block_opt =
            Cfg.find_reachable_block_opt hd state.info.block_map
          in
          match abort_block_opt with
          | None -> search_all_targets tl has_timeout
          | Some _ -> (
              let%lwt { inputss; state = dbmc_state; is_timeout; _ } =
                Dbmc.Main.main_lwt ~config:dbmc_config ~state program
              in
              match List.hd inputss with
              | Some inputs -> (
                  let () = print_endline "Lookup target: " in
                  let () = print_endline @@ Id.show hd in
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
                        let result =
                          match sato_mode with
                          | Bluejay ->
                              let errors =
                                Sato_result.Bluejay_type_errors.get_errors
                                  init_sato_state dbmc_state session final_env
                                  inputs
                              in
                              (Some (Bluejay_error errors), has_timeout)
                          | Jay ->
                              let errors =
                                Sato_result.Jay_type_errors.get_errors
                                  init_sato_state dbmc_state session final_env
                                  inputs
                              in
                              (Some (Jay_error errors), has_timeout)
                          | Jayil ->
                              let errors =
                                Sato_result.Jayil_type_errors.get_errors
                                  init_sato_state dbmc_state session final_env
                                  inputs
                              in
                              (Some (Jayil_error errors), has_timeout)
                        in
                        Lwt.return result
                    | _ -> failwith "Shoud have run into abort here!"))
              | None ->
                  if is_timeout
                  then search_all_targets tl true
                  else search_all_targets tl has_timeout)
        with ex -> (* Printexc.print_backtrace Out_channel.stderr ; *)
                   raise ex)
  in
  search_all_targets target_vars false

let main_commandline () =
  let config =
    Argparse.parse_commandline ~config:Global_config.default_sato_config ()
  in

  let program_full =
    File_utils.read_source_full ~do_wrap:config.is_wrapped
      ~do_instrument:config.is_instrumented config.filename
  in
  let () =
    let errors_res = Lwt_main.run (main_lwt ~config program_full) in
    match errors_res with
    | None, false -> print_endline @@ "No errors found."
    | None, true ->
        print_endline
        @@ "Some search timed out; inconclusive result. Please run again with \
            longer timeout setting."
    | Some errors, _ -> print_endline @@ show_reported_error errors
  in
  Log.close ()
