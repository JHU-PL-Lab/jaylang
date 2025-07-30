open Core

open Lang.Parser.Bluejay
open Tests_utils
open Utils

let make_pp_test_from_filename (testname : Filename.t) : unit Alcotest.test_case = 
  let ast1 = parse_program (In_channel.create testname) in
  make_test_case_from_ast testname parse_single_pgm_string (fun () -> ast1)

let root_dir = "test/bjy/"

let make_pp_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      >>| make_pp_test_from_filename
    )
