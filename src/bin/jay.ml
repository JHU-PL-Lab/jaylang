open Core
open Jay
open Leak

let usage_msg = "jay -i <file>"
let source_file = ref ""
let anon_fun _ = failwith "No anonymous argument allowed!"
let oc = Out_channel.create ~append:true "debug.txt"

let run_program source =
  let program = Dj_common.File_utils.parse_jay_file source in
  Jay_ast_pp.print_expr program ;
  (* Fmt.pr "\n" ; *)
  Format_helper.dump oc Fmt.stdout ;
  Fmt.pr "\n" ;
  Fmt.pr "@." ;
  Jay.Pp.print_expr program ;
  Fmt.pr "\n" ;
  Format_helper.dump oc Fmt.stdout ;
  Fmt.pr "\n" ;

  ()

let () =
  Arg.parse
    [ ("-i", Arg.Set_string source_file, "Iutput source file") ]
    anon_fun usage_msg ;
  run_program !source_file

let () = Out_channel.close oc
