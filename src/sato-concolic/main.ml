open Core
open Jayil.Ast
open Sc_result
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
    Sc_state.initialize_state_with_expr sato_mode program_full
  in
  let res =
    match config.timeout with
    | None -> Concolic.Driver.test_expr ~random:false program
    | Some t ->
        Concolic.Driver.test_expr
          ~random:false
          ~global_timeout_sec:(Core_private.Span_float.to_sec t)
          program
  in
  match res with
  | Found_abort (branch, input_lst) -> (
      let abort_var = branch.branch_ident in
      let () = print_endline "Lookup target: " in
      let () = print_endline @@ Id.show branch.branch_ident in
      let inputs =
        List.map ~f:(fun input_rec -> Some input_rec.input_value) input_lst
      in
      let session =
        {
          (From_dbmc.Interpreter.make_default_session ()) with
          input_feeder = Input_feeder.from_list inputs;
        }
      in
      try From_dbmc.Interpreter.eval session program
      with From_dbmc.Interpreter.Found_abort ab_clo -> (
        match ab_clo with
        | AbortClosure final_env ->
            (* let pp_dvalue_with_stack oc (dv, _) =
                 Dbmc.Interpreter.pp_dvalue oc dv
               in
               let () =
                 Format.printf "This is the final env: \n %a"
                   (Ident_map.pp pp_dvalue_with_stack)
                   final_env
               in *)
            let result =
              match sato_mode with
              | Bluejay ->
                  let errors =
                    Sc_result.Bluejay_type_errors.get_errors init_sato_state
                      abort_var session final_env inputs
                  in
                  (Some (Bluejay_error errors), false)
              | Jay ->
                  let errors =
                    Sc_result.Jay_type_errors.get_errors init_sato_state
                      abort_var session final_env inputs
                  in
                  (Some (Jay_error errors), false)
              | Jayil ->
                  let errors =
                    Sc_result.Jayil_type_errors.get_errors init_sato_state
                      abort_var session final_env inputs
                  in
                  (Some (Jayil_error errors), false)
            in
            Lwt.return result
        | _ -> failwith "Shoud have run into abort here!"))
  | Type_mismatch _ -> failwith "found type mismatch, but currently unhandled in sato-concolic"
  | Exhausted -> Lwt.return (None, false)
  | Exhausted_pruned_tree | Timeout -> Lwt.return (None, true)

let main_commandline () =
  let config =
    Argparse.parse_commandline ~config:Global_config.default_sato_config ()
  in
  let program_full =
    File_utils.read_source_full ~do_wrap:config.is_wrapped ~do_instrument:true (* NOTICE: Brandon changed do_instrument to false. It is typically true (but actually right now it's back to true) *)
      config.filename
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
