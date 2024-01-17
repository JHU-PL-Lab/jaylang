open Core
open Dbmc

let usage_msg = "jil -i <file> [<input_i>]"
let source_file = ref ""
let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [("-i", Arg.Set_string source_file, "Input source file") ]

let reset_limit = 5

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> Concolic_driver.test_program_concolic src_file reset_limit