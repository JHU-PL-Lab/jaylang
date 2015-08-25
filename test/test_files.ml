(**
   This test module will load a series of test files from the test sources
   directory and execute them one by one.

   Each file is expected to contain a comment describing the expected test
   result.  The comment should be of one of the following forms:

    - [EXPECT-EVALUATE] (which requires that the code evaluates to completion)
    - [EXPECT-STUCK] (which requires that the code gets stuck)
*)

open Batteries;;
open OUnit2;;

open Ast_wellformedness;;
open Interpreter;;
open String_utils;;

exception File_test_creation_failure of string;;

type test_expectation =
  | Expect_evaluate
  | Expect_stuck
;;

let parse_expectation str =
  match str with
  | "EXPECT-EVALUATE" -> Some(Expect_evaluate)
  | "EXPECT-STUCK" -> Some(Expect_stuck)
  | _ -> None
;;

let observe_evaluated expectations =
  expectations |> List.iter
    (function
      | Expect_stuck ->
        assert_failure @@
        "Evaluation completed but was expected to become stuck."
      | _ -> ()
    )
;;

let observe_stuck failure expectations =
  expectations |> List.iter
    (function
      | Expect_evaluate ->
        assert_failure @@ "Evaluation became stuck: " ^ failure
      | _ -> ()
    )
;;

let make_test filename expectations =
  let name_of_expectation expectation = match expectation with
    | Expect_evaluate ->
      "should evaluate"
    | Expect_stuck ->
      "should get stuck"
  in
  let test_name = filename ^ ": (" ^
                  pretty_list name_of_expectation expectations ^ ")"
  in
  (* Create the test in a thunk. *)
  test_name >::
  function _ ->
    (* Begin by parsing the file. *)
    let expr = File.with_file_in filename Parser.parse_program in
    (* Verify that it is well-formed. *)
    check_wellformed_expr expr;
    (* Now, based on our expectation, do the right thing. *)
    try
      ignore (eval expr);
      expectations |> observe_evaluated
    with
    | Evaluation_failure(failure) ->
      expectations |> observe_stuck failure
;;

let make_test_from filename =
  let expectations =
    filename
    |> File.lines_of
    |> Enum.filter_map
      (fun str ->
         let str' = String.trim str in
         if String.starts_with str' "#"
         then
           let str'' = String.trim @@ String.tail str' 1 in
           parse_expectation str''
         else None
      )
    |> List.of_enum
  in
  match expectations with
  | [] ->
    raise (File_test_creation_failure(
        "Could not create test from file " ^ filename ^
        ": no expectation comment found."))
  | _ ->
    make_test filename expectations
;;

let make_all_tests pathname =
  if Sys.file_exists pathname && Sys.is_directory pathname
  then
    Sys.files_of pathname
    |> Enum.map (fun f -> pathname ^ Filename.dir_sep ^ f)
    |> Enum.filter (fun f -> not @@ Sys.is_directory f)
    |> Enum.filter (fun f -> String.ends_with f ".code")
    |> Enum.map make_test_from
    |> List.of_enum
  else
    raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_source_files" >::: make_all_tests "test-sources";;