open! Core
open Jayil
open Odefa_natural
open Bluejay

let is_ton_ext s = Filename.check_suffix s "tnat"

let mode_from_file s =
  if is_ton_ext s
  then Sato_args.Typed_natodefa
  else if Odefa_natural.File_utils.check_ext s
  then Sato_args.Natodefa
  else if Jayil.File_utils.check_ext s
  then Sato_args.Odefa
  else failwith "file extension must be .odefa, .natodefa, or .tnat"

let read_source_sato filename =
  let program =
    if is_ton_ext filename
    then (
      let tnatast =
        Ton_ast.new_expr_desc
        @@ In_channel.with_file filename ~f:Bluejay.Ton_parse.parse_program_raw
      in
      let tnatast_internal = Ton_ast_internal.to_internal_expr_desc tnatast in
      let core_ast, ton_on_maps =
        (* Typed -> Untyped *)
        Ton_to_on.transform_natodefa tnatast_internal.body
      in
      let ton_on_maps' =
        Ton_to_on_maps.find_all_syn_tags ton_on_maps tnatast_internal
      in
      let natast = Ton_ast_internal.to_natodefa_expr_desc core_ast in
      (* let (desugared_typed, ton_on_maps) = transform_natodefa natast in *)
      (* let () = print_endline @@ On_ast_pp.show_expr_desc natast in *)
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        On_to_odefa.translate ~is_instrumented:true natast
      in
      let () = print_endline @@ Jayil.Ast_pp.show_expr post_inst_ast in
      Ast_wellformedness.check_wellformed_expr post_inst_ast ;
      (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, Some ton_on_maps'))
    else if Odefa_natural.File_utils.check_ext filename
    then (
      let natast =
        On_ast.new_expr_desc
        @@ In_channel.with_file filename
             ~f:Odefa_natural.On_parse.parse_program_raw
      in
      (* let (desugared_typed, ton_on_maps) = transform_natodefa natast in *)
      let post_inst_ast, odefa_inst_maps, on_odefa_maps =
        On_to_odefa.translate ~is_instrumented:true natast
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
        Jay_instrumentation.Instrumentation.instrument_odefa pre_inst_ast
      in
      (* let () = print_endline @@ Jayil.Ast_pp.show_expr post_inst_ast in *)
      Ast_wellformedness.check_wellformed_expr post_inst_ast ;
      (post_inst_ast, odefa_inst_maps, None, None))
    else failwith "file extension must be .odefa, .natodefa, or .tnat"
  in
  program
