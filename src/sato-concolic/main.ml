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

let main_lwt ~(config : Global_config.t) program_full : unit =
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
    let res = Dbmc.Concolic.test program in
    match res with
    | Found_abort branch -> failwith "TBI!"
    | _ -> print_endline @@ "No aborts found."
  in
  Log.close ()
