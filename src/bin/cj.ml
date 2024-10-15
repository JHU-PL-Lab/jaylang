open Core
open Concolic

(* This executable is to run the concolic evaluator. Think CJ = "concolic jil" *)
let usage_msg =
  {|
  cj -i <file> [-t <total timeout>] [-s <solver_timeout>] [-m <max_step>] [-d <max_tree_depth>] [-n <n_depth_increments>] [-p] [-r]
  |}

let source_file = ref "" 
let optional_args = Options.Refs.create_default ()

let read_anon_arg src_file_raw =
  source_file := src_file_raw

let speclist = 
  (* optional args for evaluation. The record fields get set by arguments *)
  [ ("-t", Arg.Set_float optional_args.global_timeout_sec, "Global timeout seconds")
  ; ("-s", Arg.Set_float optional_args.solver_timeout_sec, "Solver timeout seconds")
  ; ("-m", Arg.Set_int   optional_args.global_max_step   , "Global max step")
  ; ("-d", Arg.Set_int   optional_args.max_tree_depth    , "Max tree depth")
  ; ("-r", Arg.Set       optional_args.random            , "Random")
  ; ("-n", Arg.Set_int   optional_args.n_depth_increments, "Num depth increments")] 

let () = 
  Arg.parse speclist read_anon_arg usage_msg;
  match !source_file with
  | "" -> ()
  | src_file ->
    let _ =
      Options.Fun.appl
        Driver.test
        (Options.Refs.without_refs optional_args)
        src_file
    in
    ()
