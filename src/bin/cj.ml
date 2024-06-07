open Core
open Dbmc

(* This executable is to run the concolic evaluator. Think CJ = "concolic jil" *)
let usage_msg =
  {|
  cj -i <file> [-t <total timeout>] [-s <solver_timeout>] [-m <max_step>] [-d <max_tree_depth>] [-q] [-p] [-r]
  |}
let source_file = ref "" 
(* let out_file = ref "" *)
let optional_args = Concolic_options.Refs.create_default ()

let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [ ("-i", Arg.Set_string source_file, "Input source file")
  (* optional args for evaluation. The record fields get set by arguments *)
  ; ("-t", Arg.Set_float optional_args.global_timeout_sec, "Global timeout seconds")
  ; ("-s", Arg.Set_float optional_args.solver_timeout_sec, "Solver timeout seconds")
  ; ("-m", Arg.Set_int   optional_args.global_max_step   , "Global max step")
  ; ("-q", Arg.Set       optional_args.quit_on_abort     , "Quit on first abort")
  ; ("-d", Arg.Set_int   optional_args.max_tree_depth    , "Max tree depth")
  ; ("-r", Arg.Set       optional_args.random            , "Random")] 

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    let _ =
      Concolic_options.Fun.appl
        Concolic_driver.test
        (Concolic_options.Refs.without_refs optional_args)
        src_file
    in
    ()
    end
