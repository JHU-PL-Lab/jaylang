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

open Ast;;
open Ast_wellformedness;;
open Ddpa_graph;;
open Interpreter;;
open String_utils;;

exception File_test_creation_failure of string;;

type test_expectation =
  | Expect_evaluate
  | Expect_stuck
  | Expect_well_formed
  | Expect_ill_formed
  | Expect_analysis_stack_is of
    (module Analysis_context_stack.Context_stack) option
  | Expect_analysis_variable_lookup_from_end of ident * string
  | Expect_analysis_inconsistency_at of ident
  | Expect_analysis_no_inconsistencies
;;

type expectation_parse =
  | Success of test_expectation
  | Failure of string
;;

exception Expectation_parse_failure of string;;

exception Expectation_not_found;;

type expectation_stack_decision =
  | Default_stack
  | Chosen_stack of (module Analysis_context_stack.Context_stack) option
;;

let parse_expectation str =
  let assert_no_args lst =
    if List.is_empty lst
    then ()
    else raise @@ Expectation_parse_failure "expected no arguments"
  in
  let assert_one_arg lst =
    match lst with
    | [x] -> x
    | _ ->
      raise @@
        Expectation_parse_failure ("expected one argument; got " ^
          string_of_int (List.length lst))
  in
  let assert_two_args lst =
    match lst with
    | [x;y] -> (x,y)
    | _ ->
      raise @@
        Expectation_parse_failure ("expected two arguments; got " ^
          string_of_int (List.length lst))
  in
  try
    let expectation =
      match String_utils.whitespace_split ~max:2 str with
      | "EXPECT-EVALUATE"::args_part ->
        assert_no_args args_part;
        Expect_evaluate
      | "EXPECT-STUCK"::args_part -> 
        assert_no_args args_part;
        Expect_stuck
      | "EXPECT-WELL-FORMED"::args_part ->
        assert_no_args args_part;
        Expect_well_formed
      | "EXPECT-ILL-FORMED"::args_part ->
        assert_no_args args_part;
        Expect_ill_formed
      | "EXPECT-ANALYSIS-STACK-IS"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split args_str in
        let name = assert_one_arg args in
        begin
          try
            let stack_module = Toploop_ddpa.stack_from_name name in
            Expect_analysis_stack_is stack_module
          with
          | Not_found ->
            raise @@ Expectation_parse_failure "invalid stack name"
        end
      | "EXPECT-ANALYSIS-LOOKUP-FROM-END"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split ~max:2 args_str in
        let (ident_str, pp_expectation) = assert_two_args args in
        let ident = Ident(ident_str) in
        Expect_analysis_variable_lookup_from_end(ident,pp_expectation)
      | "EXPECT-ANALYSIS-INCONSISTENCY-AT"::args_part ->
        let args_str = String.join "" args_part in
        let args = whitespace_split args_str in
        let call_site = assert_one_arg args in
        Expect_analysis_inconsistency_at (Ident(call_site))
      | "EXPECT-ANALYSIS-NO-INCONSISTENCIES"::args_part ->
        assert_no_args args_part;
        Expect_analysis_no_inconsistencies
      | _ ->
        raise @@ Expectation_not_found
    in
    Some (Success expectation)
  with
  | Expectation_parse_failure s -> Some (Failure s)
  | Expectation_not_found -> None
;;

let observe_evaluated expectation =
  match expectation with
  | Expect_evaluate -> None
  | Expect_stuck ->
    assert_failure @@ "Evaluation completed but was expected to become stuck."
  | _ -> Some expectation
;;

let observe_stuck failure expectation =
  match expectation with
  | Expect_evaluate ->
    assert_failure @@ "Evaluation became stuck: " ^ failure
  | Expect_stuck -> None
  | _ -> Some expectation
;;

let observe_well_formed expectation =
  match expectation with
  | Expect_well_formed -> None
  | Expect_ill_formed ->
    assert_failure @@ "Well-formedness check passed but was expect to fail."
  | _ -> Some expectation
;;

let observe_ill_formed illformednesses expectation =
  match expectation with
  | Expect_well_formed -> 
    assert_failure @@ "Expression was unexpectedly ill-formed.  Causes:" ^
                      "\n    * " ^ concat_sep "\n    *"
                        (List.enum @@
                         List.map pretty_illformedness illformednesses)
  | Expect_ill_formed -> None
  | _ -> Some expectation
;;

let observe_analysis_stack_selection chosen_stack_ref expectation =
  match expectation with
  | Expect_analysis_stack_is module_option ->
    begin
      chosen_stack_ref :=
        begin
          match !chosen_stack_ref with
          | Default_stack -> Chosen_stack module_option
          | Chosen_stack _ ->
            assert_failure @@ "multiple expectations of analysis stack"
        end;
      None
    end
  | _ -> Some expectation
;;

let observe_analysis_variable_lookup_from_end ident repr expectation =
  match expectation with
  | Expect_analysis_variable_lookup_from_end(ident',repr') ->
    if ident = ident'
    then
      begin
        if repr = repr'
        then None
        else assert_failure @@
          Printf.sprintf "for variable %s, expected %s but got %s"
            (pretty_ident ident) repr' repr
      end
    else Some expectation
  | _ -> Some expectation
;;

let observe_inconsistency inconsistency expectation =
  match inconsistency with
  | Toploop_ddpa.Application_of_non_function(_,Clause(Var(ident,_),_),_) ->
    begin
      match expectation with
      | Expect_analysis_inconsistency_at ident' ->
        if ident = ident'
        then None
        else Some expectation
      | _ -> Some expectation
    end
;;

let observe_no_inconsistency expectation =
  match expectation with
  | Expect_analysis_no_inconsistencies -> None
  | _ -> Some expectation
;;

let make_test filename expectations =
  let name_of_expectation expectation = match expectation with
    | Expect_evaluate -> "should evaluate"
    | Expect_stuck -> "should get stuck"
    | Expect_well_formed -> "should be well-formed"
    | Expect_ill_formed -> "should be ill-formed"
    | Expect_analysis_stack_is stack_option ->
      let name =
        match stack_option with
        | Some stack ->
          let module Stack =
            (val stack : Analysis_context_stack.Context_stack)
          in
          Stack.name
        | None -> "none"
      in
      "should use analysis stack " ^ name
    | Expect_analysis_variable_lookup_from_end(ident,_) ->
      "should have particular values for variable " ^ (pretty_ident ident)
    | Expect_analysis_inconsistency_at ident ->
      "should be inconsistent at " ^ pretty_ident ident
    | Expect_analysis_no_inconsistencies ->
      "should be consistent"
  in
  let test_name = filename ^ ": (" ^
                  pretty_list name_of_expectation expectations ^ ")"
  in
  (* Create the test in a thunk. *)
  test_name >::
  function _ ->
    (* Using a mutable list of not-yet-handled expectations. *)
    let expectations_left = ref expectations in
    (* This routine takes an observation function and applies it to all of the
       not-yet-handled expectations. *)
    let observation f =
      expectations_left := List.filter_map f @@ !expectations_left
    in
    (* This routine detects expectations of a particular form. *)
    let have_expectation pred = List.exists pred (!expectations_left) in
    (* We're going to execute the following block.  If it completes without
       error, we're also going to require that all of its expectations were
       satisfied.  This addresses nonsense cases such as expecting an ill-formed
       expression to evaluate. *)
    begin
      let expr = File.with_file_in filename Parser.parse_program in
      (* Confirm well-formedness. *)
      begin
        try
          check_wellformed_expr expr;
          observation observe_well_formed
        with
        | Illformedness_found(illformednesses) ->
          observation (observe_ill_formed illformednesses)
      end;
      (* Decide what kind of analysis to perform. *)
      let module_choice = ref Default_stack in
      observation (observe_analysis_stack_selection module_choice);
      let chosen_module_option =
        match !module_choice with
        | Default_stack ->
          Some (module Analysis_single_element_stack.Stack :
                  Analysis_context_stack.Context_stack)
        | Chosen_stack value -> value
      in
      (* If the test wants an analysis, build it and work through observations
         now. *)
      match chosen_module_option with
      | None -> ()
      | Some chosen_module ->
        begin
          let module Stack = (val chosen_module) in
          let module A = Analysis.Make(Stack) in
          let module TLA = Toploop_ddpa.Make(A) in
          (* Create the analysis now. *)
          let analysis = TLA.create_analysis expr in
          (* If we have any expectations about inconsistency, we should observe
             them now. *)
          if not @@ have_expectation
              (function
                | Expect_analysis_no_inconsistencies -> true
                | Expect_analysis_inconsistency_at _ -> true
                | _ -> false)
          then ()
          else 
            begin
              let inconsistencies = TLA.check_inconsistencies analysis in
              (* Report the lack of inconsistencies, when appropriate. *)
              if Enum.is_empty inconsistencies
              then observation observe_no_inconsistency;
              (* Report each inconsistency. *)
              inconsistencies
              |> Enum.iter
                (fun inconsistency ->
                  observation @@ observe_inconsistency inconsistency);
              (* If there are any expectations of inconsistency left, they're
                 a problem. *)
              !expectations_left
              |> List.iter
                (function
                  | Expect_analysis_inconsistency_at ident ->
                    assert_failure @@ "Expected inconsistency at " ^
                      pretty_ident ident ^ " which did not occur"
                  | _ -> ()
                )
            end
          ;
          (* We only want to observe variable values for which there is an
             expectation. *)
          let variable_idents =
            !expectations_left
            |> List.enum
            |> Enum.filter_map
              (function
                | Expect_analysis_variable_lookup_from_end(ident,_) ->
                  Some ident
                | _ -> None)
          in
          (* For each such variable, perform an appropriate observation. *)
          Enum.iter
            (fun ident ->
              let var = Var(ident,None) in
              let values =
                TLA.values_of_variable_from var End_clause analysis
              in
              let repr = pp_abs_value_set values in
              observation (observe_analysis_variable_lookup_from_end ident repr)
            ) variable_idents
        end
      ;
      (* Evaluate, but only if an appropriate expectation exists. *)
      if not @@ have_expectation
                  (function
                    | Expect_evaluate -> true
                    | Expect_stuck -> true
                    | _ -> false)
      then ()
      else
        begin
          try
            ignore (eval expr);
            observation observe_evaluated
          with
          | Evaluation_failure(failure) ->
            observation (observe_stuck failure)
        end
      ;
    end;
    (* Now assert that every expectation has been addressed. *)
    match !expectations_left with
    | [] -> ()
    | expectations' ->
      assert_failure @@ "The following expectations could not be met:" ^
                        "\n    * " ^ concat_sep "\n    *"
                          (List.enum @@
                           List.map name_of_expectation expectations')
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
          match parse_expectation str'' with
          | Some (Success expectation) -> Some(Success expectation)
          | Some (Failure s) -> Some(Failure(
            Printf.sprintf
              "Error parsing expectation:\n        Error: %s\n        Text:  %s"
              s str''))
          | None -> None
        else None
      )
    |> List.of_enum
  in
  let failures =
    expectations
    |> List.filter_map
      (function
        | Success _ -> None
        | Failure s -> Some s
      )
  in
  match failures with
  | [] ->
    let successes =
      expectations
      |> List.filter_map
        (function
          | Success expectation -> Some expectation
          | Failure _ -> None
        )
    in
    begin
      match successes with
      | [] -> raise (File_test_creation_failure(
        "Could not create test from file " ^ filename ^
        ": no expectations found"))
      | _ ->
        make_test filename successes
    end
  | _ ->
    let message = "Could not create test from file " ^ filename ^ ":" in
    let message' =
      failures
      |> List.fold_left
        (fun msg err -> msg ^ "\n    " ^ err) message
    in
    raise (File_test_creation_failure message')
;;

let wrap_make_test_from filename =
  try
    make_test_from filename
  with
  | File_test_creation_failure s ->
    filename >:: function _ -> assert_failure s
;;

let make_all_tests pathname =
  if Sys.file_exists pathname && Sys.is_directory pathname
  then
    Sys.files_of pathname
    |> Enum.map (fun f -> pathname ^ Filename.dir_sep ^ f)
    |> Enum.filter (fun f -> not @@ Sys.is_directory f)
    |> Enum.filter (fun f -> String.ends_with f ".code")
    |> Enum.map wrap_make_test_from
    |> List.of_enum
  else
    raise (File_test_creation_failure(
        "Test file directory " ^ pathname ^ " is missing"))
;;

let tests = "Test_source_files" >::: make_all_tests "test-sources";;
