
open Core

let usage_msg =
  {|
  fuzzer <file>
  |}

let source_file = ref "" 

let anon_fun src_file_name =
  source_file := src_file_name

let speclist = 
  []

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    let expr =
      Dj_common.Convert.jil_ast_of_convert
      @@ Dj_common.File_utils.read_source_full src_file ~do_wrap:true ~do_instrument:true
    in
    if
      Fuzz.Interp.test
        expr
        Int.(10 ** 4)
    then
      Format.printf "Found failure\n"
    else
      Format.printf "No failure found\n"
  end