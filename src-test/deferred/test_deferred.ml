open Core
open Lang
open Utils
open Deferred

(*
  This just runs some acceptance tests on the deferred interpreter
  over the embedded target program.
  * A well-typed program never hits an error
  * An ill-typed program may or may not hit an error
*)
let testcase_of_filename (testname : Filename.t) : unit Alcotest.test_case = 
  let metadata = Metadata.of_bjy_file testname in
  let is_error_expected = 
    match metadata.typing with
    | Ill_typed -> true
    | Well_typed -> false
  in
  let speed_level =
    match metadata.speed with
    | Slow -> `Slow
    | Fast -> `Quick
  in
  Alcotest.test_case testname speed_level
  @@ fun () ->
  let bjy =
    Parser.Bluejay.parse_single_pgm_string @@ In_channel.read_all testname
  in
  let emb = Translate.Convert.bjy_to_emb ~do_wrap:true ~do_type_splay:false bjy in
  let is_error =
    match Main.deval emb with
    | Ok _
    | Error `XVanish _
    | Error `XReach_max_step _ -> false
    | Error _ -> true
  in
  if is_error_expected 
  then (* any result okay because this run may or may not hit error *)
    Alcotest.check Alcotest.pass "deval" () ()
  else (* definitely should not have hit the error *)
    Alcotest.check Alcotest.bool "deval" true (not is_error)

let root_dir = "test/deferred/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
  ( dirname
  , [ root_dir ^ dirname ]
    |> File_utils.get_all_bjy_files
    >>| testcase_of_filename
  )

let () =
  Alcotest.run "deferred"
  @@ make_tests
    [ "basic"
    ; "recursive"
    ]

