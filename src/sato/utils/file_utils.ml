open! Core
open Jayil
open Jay
open Bluejay
open Jay_translate
open Dj_common

let print_delimiter_line content =
  print_endline "*************************************" ;
  print_endline content

let read_source_sato ?(do_wrap = false) ?(do_instrument = true) filename =
  let post_inst_ast, odefa_inst_maps, on_odefa_maps, ton_on_maps' =
    if File_utils.check_bluejay_ext filename
    then (
      let raw_bluejay = File_utils.parse_bluejay_file filename in
      let bluejay_ast = Bluejay_ast.new_expr_desc raw_bluejay in

      let core_ast, ton_on_maps =
        Convert.bluejay_edesc_to_core_ast ~do_wrap bluejay_ast
      in
      (* let bluejay_ast_internal =
           Convert.raw_bluejay_to_jayil ~do_wrap ~do_instrument raw_bluejay
         in *)
      (* print_delimiter_line "Original program: " ;
         let () = print_endline @@ Bluejay_ast.show_expr_desc bluejay_ast in *)
      let init_consts = Convert.bluejay_edesc_to_consts bluejay_ast in
      (* let bluejay_ast_internal =
           Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
         in
         let core_ast, ton_on_maps =
           (* Typed -> Untyped *)
           Bluejay_to_jay.transform_bluejay ~do_wrap bluejay_ast_internal.body
         in *)
      let ton_on_maps' =
        let bluejay_ast_internal =
          Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
        in
        Bluejay_to_jay_maps.find_all_syn_tags ton_on_maps bluejay_ast_internal
      in
      let jay_ast = Bluejay_ast_internal.to_jay_expr_desc core_ast in
      print_delimiter_line "Jay program after type elimination: " ;
      let () = print_endline @@ Jay_ast_pp.show_expr_desc jay_ast in
      let instrument_tags_bluejay = ton_on_maps'.instrumented_tags in
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        Jay_translate.Jay_to_jayil.translate ~is_jay:true
          ~is_instrumented:do_instrument ~consts:init_consts
          ~bluejay_instruments:instrument_tags_bluejay jay_ast
      in
      print_delimiter_line "Jayil program after instrumentation: " ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, Some ton_on_maps'))
    else if File_utils.check_jay_ext filename
    then (
      let jay_ast =
        Jay_ast.new_expr_desc @@ File_utils.parse_jay_file filename
      in
      let consts = Convert.jay_edesc_to_consts jay_ast in
      print_delimiter_line "Jay program before instrumentation: " ;
      let () = print_endline @@ Jay_ast_pp.show_expr_desc jay_ast in
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        Jay_to_jayil.translate ~is_instrumented:do_instrument ~consts jay_ast
      in
      print_delimiter_line "Jayil program after instrumentation: " ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, None))
    else if File_utils.check_jayil_ext filename
    then
      let pre_inst_ast = File_utils.parse_jayil_file filename in
      let post_inst_ast, odefa_inst_maps =
        if do_instrument
        then Jay_instrumentation.Instrumentation.instrument_jayil pre_inst_ast
        else
          ( pre_inst_ast,
            Jay_instrumentation.Jayil_instrumentation_maps.empty false )
      in
      (post_inst_ast, odefa_inst_maps, None, None)
    else failwith "file extension must be .jil, .jay, or .bjy"
  in
  let () = Fmt.pr "%a" Jayil.Pp.expr post_inst_ast in
  Ast_wellformedness.check_wellformed_expr post_inst_ast ;
  Fmt.pr "@?" ;
  (post_inst_ast, odefa_inst_maps, on_odefa_maps, ton_on_maps')
