
open Core

let usage_msg =
  {|
  fuzzer -i <file>
  |}

let source_file = ref "" 

let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [ ("-i", Arg.Set_string source_file, "Input source file") ]

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