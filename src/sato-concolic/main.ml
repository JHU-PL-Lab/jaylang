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

  let () =
    let res, has_pruned =
      Lwt_main.run
      @@ Dbmc.Concolic_options.Fun.appl Dbmc.Concolic.lwt_eval
           (Dbmc.Concolic_options.Refs.without_refs
              (Dbmc.Concolic_options.Refs.create_default ()))
           program
    in
    match errors_res with
    | None, false -> print_endline @@ "No errors found."
    | None, true ->
        print_endline
        @@ "Some search timed out; inconclusive result. Please run again with \
            longer timeout setting."
    | Some errors, _ -> print_endline @@ show_reported_error errors
  in
  Log.close ()
