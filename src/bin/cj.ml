open Core
open Dbmc

let usage_msg = "jil -i <file> [<input_i>]"
let source_file = ref ""
let timeout_sec = ref (Float.max_value)
let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [ ("-i", Arg.Set_string source_file, "Input source file")
  ; ("-t", Arg.Set_float timeout_sec, "Timeout seconds") ]

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    if Float.(!timeout_sec = max_value)
    then Concolic_driver.test_program_concolic src_file
    else Concolic_driver.test_program_concolic ~timeout_sec:(!timeout_sec) src_file
    end
