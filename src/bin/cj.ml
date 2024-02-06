open Core
open Dbmc

(* This executable is to run the concolic evaluator. Think CJ = "concolic jil" *)
let usage_msg = "jil -i <file> [-t <total timeout>] [-s <solver_timeout>] [-m <max_step>] [-q]"
let source_file = ref ""
let global_timeout_sec = ref 0.0
let solver_timeout_sec = ref 0.0
let global_max_step = ref 0
let quit_on_first_abort = ref false
let version = ref ""
let inputs = ref []

let anon_fun i_raw =
  let this_i = int_of_string_opt i_raw in
  inputs := !inputs @ [ this_i ]

let speclist = 
  [ ("-i", Arg.Set_string source_file, "Input source file")
  ; ("-t", Arg.Set_float global_timeout_sec, "Global timeout seconds")
  ; ("-s", Arg.Set_float solver_timeout_sec, "Solver timeout seconds")
  ; ("-m", Arg.Set_int global_max_step, "Global max step")
  ; ("-q", Arg.Set quit_on_first_abort, "Quit on first abort")
  ; ("-v", Arg.Set_string version, "Version") ]

let () = 
  Arg.parse speclist anon_fun usage_msg;
  match !source_file with
  | "" -> ()
  | src_file -> begin
    let f =
      match !version with
      | "strict" -> Concolic_driver.test_program_strict_concolic
      | "loose" | "" -> Concolic_driver.test_program_concolic
      | _ -> failwith "unknown version"
    in
    let _ =
    f
      src_file
      ?global_timeout_sec:(Option.some_if Float.(!global_timeout_sec <> 0.0) !global_timeout_sec)
      ?solver_timeout_sec:(Option.some_if Float.(!solver_timeout_sec <> 0.0) !solver_timeout_sec)
      ?global_max_step:(Option.some_if (!global_max_step <> 0) !global_max_step)
      ~quit_on_first_abort:!quit_on_first_abort
    in
    ()
    end
