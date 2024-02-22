open Core
open Dbmc

(* This executable is to run the concolic evaluator. Think CJ = "concolic jil" *)
let usage_msg = "jil -i <file> [-t <total timeout>] [-s <solver_timeout>] [-m <max_step>] [-q]"
let source_file = ref "" 
let version = ref "strict" (* default *)
let optional_args = Concolic_options.Refs.create_default ()

let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [ ("-i", Arg.Set_string source_file, "Input source file")
  ; ("-v", Arg.Set_string version, "Version: strict or loose")
  (* optional args for evaluation *)
  ; ("-t", Arg.Set_float optional_args.global_timeout_sec, "Global timeout seconds")
  ; ("-s", Arg.Set_float optional_args.solver_timeout_sec, "Solver timeout seconds")
  ; ("-m", Arg.Set_int optional_args.global_max_step, "Global max step")
  ; ("-q", Arg.Set optional_args.quit_on_abort, "Quit on first abort")
  ; ("-p", Arg.Set optional_args.print_solver, "Print solver")
  ; ("-d", Arg.Set_int optional_args.max_tree_depth, "Max tree depth") ] 

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    let f =
      match !version with
      | "strict" -> Concolic_driver.test_program_concolic
      | "loose" -> Concolic_driver.test_program_loose_concolic
      | _ -> failwith "unknown version"
    in
    let g =
      Concolic_options.F.appl f
      @@ Concolic_options.Refs.without_refs optional_args
    in
    let _ = g src_file in
    ()
    end
