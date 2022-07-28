open! Core
open Odefa_ast;;

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
      failwith "To be implemented!"
      (* begin
        let natast =
          In_channel.with_file filename
            ~f:Odefa_natural.On_parse.parse_program_raw
        in
        let (desugared_typed, ton_on_maps) = transform_natodefa natast in
        let (odefa_ast, on_odefa_maps) =
          On_to_odefa.translate ~is_instrumented:true desugared_typed 
        in
        Ast_wellformedness.check_wellformed_expr odefa_ast;
        (odefa_ast, on_odefa_maps, Some ton_on_maps)
      end *)
    else 
      if is_odefa_ext filename
      then 
        let pre_inst_ast = 
          In_channel.with_file filename ~f:Odefa_parser.Parse.parse_program_raw
        in
        let (post_inst_ast, on_odefa_maps) =
          Odefa_natural.Odefa_instrumentation.instrument_odefa pre_inst_ast
        in
        (* let () = print_endline @@ Odefa_ast.Ast_pp.show_expr post_inst_ast in *)
        Ast_wellformedness.check_wellformed_expr post_inst_ast;
        (pre_inst_ast, post_inst_ast, on_odefa_maps, None)
      else failwith "file extension must be .odefa or .natodefa"
  in
  program

