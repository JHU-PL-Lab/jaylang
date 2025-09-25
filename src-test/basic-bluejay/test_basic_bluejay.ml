open Core
open Concolic
open Utils

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
    Cmdliner.Cmd.eval_value' ~argv:(Array.append [| ""; testname; "-t"; "10.0" |] metadata.flags) Driver.ceval
    |> begin function
      | `Ok status -> Status.is_error_found status
      | `Exit i -> raise @@ Invalid_argument (Format.sprintf "Test couldn't evaluate and finished with exit code %d." i)
    end
    |> Bool.(=) is_error_expected
    |> Alcotest.check Alcotest.bool "bjy concolic" true

let root_dir = "test/bjy/"

let make_tests (dirs : string list) : unit Alcotest.test list =
  let open List.Let_syntax in
  dirs >>| fun dirname -> 
    ( dirname
    , [ root_dir ^ dirname ]
      |> File_utils.get_all_bjy_files
      >>| testcase_of_filename
    )

let () =
  Alcotest.run "concolic"
  @@ make_tests [ "navya-tests" ]
    
