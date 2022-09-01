open Core

let check_wellformed_or_exit ast =
  let open Jayil in
  try Ast_wellformedness.check_wellformed_expr ast
  with Ast_wellformedness.Illformedness_found ills ->
    print_endline "Program is ill-formed." ;
    ills
    |> List.iter ~f:(fun ill ->
           print_string "* " ;
           print_endline @@ Ast_wellformedness.show_illformedness ill) ;
    ignore @@ Stdlib.exit 1

let load filename =
  let program =
    if String.is_suffix filename ~suffix:"natodefa"
    then
      let natast =
        Exn.handle_uncaught_and_exit (fun () ->
            In_channel.with_file filename ~f:Jay.Jay_parse.parse_program_raw)
      in
      Jay.Jay_to_jayil.translate natast
    else if String.is_suffix filename ~suffix:"odefa"
    then In_channel.with_file filename ~f:Jayil_parser.Parser.parse_program_raw
    else failwith "file extension must be .odefa or .natodefa"
  in
  ignore @@ check_wellformed_or_exit program ;
  program
