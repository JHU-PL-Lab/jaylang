open Core
open Jay

let usage_msg = "jay -i <file>"
let source_file = ref ""
let anon_fun _ = failwith "No anonymous argument allowed!"

let run_program source =
  let program = Dj_common.File_utils.parse_jay_file source in
  Jay_ast_pp.print_expr program ;
  Fmt.pr "\n----\n@." ;
  Jay.Pp.print_expr program

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Iutput source file") ]
    anon_fun usage_msg ;
  run_program !source_file
