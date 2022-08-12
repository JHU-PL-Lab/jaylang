open! Core
open Odefa_ast
open Odefa_natural

(* Probably should be moved to a more general util file for all systems? *)
let is_odefa_ext s = Filename.check_suffix s "odefa"
(* || Filename.check_suffix filename "natodefa"  *)
(* String.is_suffix s ~suffix:"odefa" *)

let is_natodefa_ext s = Filename.check_suffix s "natodefa"
(* String.is_suffix s ~suffix:"natodefa" *)

let read_source_sato filename =
  let program =
    if is_natodefa_ext filename
    then
      begin
        let natast = On_ast.new_expr_desc @@
          In_channel.with_file filename
            ~f:Odefa_natural.On_parse.parse_program_raw
        in
        (* let (desugared_typed, ton_on_maps) = transform_natodefa natast in *)
        let (post_inst_ast, odefa_inst_maps, on_odefa_maps) =
          On_to_odefa.translate ~is_instrumented:true natast 
        in
        let () = print_endline @@ Odefa_ast.Ast_pp.show_expr post_inst_ast in
        Ast_wellformedness.check_wellformed_expr post_inst_ast;
        (post_inst_ast, odefa_inst_maps, Some on_odefa_maps, None)
      end
    else 
      if is_odefa_ext filename
      then 
        let pre_inst_ast = 
          In_channel.with_file filename ~f:Odefa_parser.Parse.parse_program_raw
        in
        let (post_inst_ast, odefa_inst_maps) =
          Odefa_instrumentation.Instrumentation.instrument_odefa pre_inst_ast
        in
        let () = print_endline @@ Odefa_ast.Ast_pp.show_expr post_inst_ast in
        Ast_wellformedness.check_wellformed_expr post_inst_ast;
        (post_inst_ast, odefa_inst_maps, None, None)
      else failwith "file extension must be .odefa or .natodefa"
  in
  program

