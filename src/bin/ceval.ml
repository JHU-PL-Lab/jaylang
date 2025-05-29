(**
  Bin [ceval].

  This executable runs the concolic evaluator on the Bluejay
  program at the given file path.
*)

open Core
open Concolic

let usage_msg =
  {|
  ceval <file> [-t <total timeout>] [-m <max_step>] [-d <max_tree_depth>] [-n <n_depth_increments>] [-r] [-w yes/no] [-p] [-s]
  |}

let source_file = ref "" 
let optional_args = Options.Refs.create_default ()
let wrap = ref "yes"
let type_splay = ref false

let read_anon_arg src_file_raw =
  source_file := src_file_raw

let speclist = 
  (* optional args for evaluation. The record fields get set by arguments *)
  [ ("-t", Arg.Set_float optional_args.global_timeout_sec, "Global timeout seconds")
  ; ("-m", Arg.Set_int   optional_args.global_max_step   , "Global max step")
  ; ("-d", Arg.Set_int   optional_args.max_tree_depth    , "Max tree depth")
  ; ("-r", Arg.Set       optional_args.random            , "Random")
  ; ("-n", Arg.Set_int   optional_args.n_depth_increments, "Num depth increments")
  ; ("-p", Arg.Set       optional_args.in_parallel       , "Run checks in parallel")
  ; ("-w", Arg.Set_string wrap, "Wrap flag: yes or no. Default is yes.")
  ; ("-s", Arg.Set type_splay, "Splay types on recursive functions")
  ]

let () = 
  Arg.parse speclist read_anon_arg usage_msg;
  match !source_file with
  | "" -> ()
  | src_file ->
    let do_wrap =
      match String.lowercase !wrap with
      | "yes" | "y" -> true
      | "no" | "n" -> false
      | _ -> Format.eprintf "Error: bad string given to wrap -w flag. Should be yes/no."; assert false
    in
    let _ =
      Options.Arrow.appl
        Driver.test
        (Options.Refs.without_refs optional_args)
        src_file
        ~do_wrap
        ~do_type_splay:!type_splay
    in
    ()
