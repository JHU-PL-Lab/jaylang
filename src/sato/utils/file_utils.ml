open! Core
open Jayil
open Jay
open Bluejay
open Jay_translate

let is_bluejay_ext s =
  Filename.check_suffix s "tnat" || Filename.check_suffix s "bjy"

let mode_from_file s =
  if is_bluejay_ext s
  then Sato_args.Bluejay
  else if Jay.File_utils.check_ext s
  then Sato_args.Jay
  else if Jayil.File_utils.check_ext s
  then Sato_args.Jayil
  else failwith "file extension must be .jil, .jay, or .bjy"

let read_source_sato filename =
  let program =
    if is_bluejay_ext filename
    then (
      let bluejay_ast =
        Bluejay_ast.new_expr_desc
        @@ In_channel.with_file filename
             ~f:Bluejay.Bluejay_parse.parse_program_raw
      in
      let bluejay_ast_internal =
        Bluejay_ast_internal.to_internal_expr_desc bluejay_ast
      in
      let core_ast, ton_on_maps =
        (* Typed -> Untyped *)
        Bluejay_to_jay.transform_bluejay bluejay_ast_internal.body
      in
      let ton_on_maps' =
        Bluejay_to_jay_maps.find_all_syn_tags ton_on_maps bluejay_ast_internal
      in
      let jay_ast = Bluejay_ast_internal.to_jay_expr_desc core_ast in
      (* let (desugared_typed, ton_on_maps) = transform_natodefa jay_ast in *)
      (* let () = print_endline @@ Jay_ast_pp.show_expr_desc jay_ast in *)
      let translation_context =
        Some
          (Jay_translate.Jay_to_jayil_monad.new_translation_context ~is_jay:true
             ~suffix:"___" ())
      in
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        Jay_translate.Jay_to_jayil.translate ~translation_context
          ~is_instrumented:true jay_ast
      in
      let () = print_endline @@ Jayil.Ast_pp.show_expr post_inst_ast in
      Ast_wellformedness.check_wellformed_expr post_inst_ast ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, Some ton_on_maps'))
    else if Jay.File_utils.check_ext filename
    then (
      let jay_ast =
        Jay_ast.new_expr_desc
        @@ In_channel.with_file filename ~f:Jay.Jay_parse.parse_program_raw
      in
      (* let (desugared_typed, ton_on_maps) = transform_natodefa jay_ast in *)
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        Jay_to_jayil.translate ~is_instrumented:true jay_ast
      in
      (* let () = print_endline @@ Jayil.Ast_pp.show_expr post_inst_ast in *)
      Ast_wellformedness.check_wellformed_expr post_inst_ast ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, None))
    else if Jayil.File_utils.check_ext filename
    then (
      let pre_inst_ast =
        In_channel.with_file filename ~f:Jayil_parser.Parse.parse_program_raw
      in
      let post_inst_ast, odefa_inst_maps =
        Jay_instrumentation.Instrumentation.instrument_jayil pre_inst_ast
      in
      (* let () = print_endline @@ Jayil.Ast_pp.show_expr post_inst_ast in *)
      Ast_wellformedness.check_wellformed_expr post_inst_ast ;
      (post_inst_ast, odefa_inst_maps, None, None))
    else failwith "file extension must be .jil, .jay, or .bjy"
  in
  program
