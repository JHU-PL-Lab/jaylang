open Core
open Dj_common

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

let load filename = File_utils.read_source filename