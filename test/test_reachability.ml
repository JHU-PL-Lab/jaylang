(**
  This test module performs a series of operations to test the PDA reachability
  functionality in the Odefa analysis library.
*)

open Batteries;;
open OUnit2;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_reachability";;

open Pds_reachability_types_stack;;

module Test_state_ord =
struct
  type t = int
  let compare = compare
end;;

module Test_stack_element_ord =
struct
  type t = char
  let compare = compare
end;;

module Test_spec =
struct
  type state = int
  type stack_element = char
  module State_ord = Test_state_ord
  module Stack_element_ord = Test_stack_element_ord
  let pp_state = string_of_int
  let pp_stack_element = String.make 1
end;;

module Test_dph =
struct
  type stack_element = Test_spec.stack_element;;
  let compare_stack_element = Test_stack_element_ord.compare;;
  type dynamic_pop_action =
    | Double_push
    | Consume_identical_1_of_2
    | Consume_identical_2_of_2 of stack_element
    [@@deriving ord]
  ;;
  let pp_dynamic_pop_action = function
    | Double_push -> "Double_push"
    | Consume_identical_1_of_2 -> "Consume_identical_1_of_2"
    | Consume_identical_2_of_2(k) ->
      Printf.sprintf "Consume_identical_2_of_2(%s)"
        (Test_spec.pp_stack_element k)
  let perform_dynamic_pop element action =
    match action with
    | Double_push -> Enum.singleton [Push(element);Push(element)]
    | Consume_identical_1_of_2 -> Enum.singleton
        [Pop_dynamic(Consume_identical_2_of_2 element)]
    | Consume_identical_2_of_2 element' ->
        if Test_stack_element_ord.compare element element' == 0
        then Enum.singleton []
        else Enum.empty ()
end;;

module Test_reachability = Pds_reachability.Make(Test_spec)(Test_dph);;

let immediate_reachability_test =
  "immediate_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Pop 'a'] 1
      |> Test_reachability.add_start_state 0 'a'
    in
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.of_enum states) [1] 
;;

let immediate_non_reachable_test =
  "immediate_non_reachable_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Pop 'b'] 1
      |> Test_reachability.add_start_state 0 'a'
    in
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.of_enum states) [] 
;;

let two_step_reachability_test =
  "two_step_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Pop 'a'; Push 'b'] 1
      |> Test_reachability.add_edge 1 [Pop 'b'] 2
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.of_enum states) [2] 
;;

let cycle_reachability_test =
  "cycle_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Push 'b'] 0
      |> Test_reachability.add_edge 0 [Push 'c'] 1
      |> Test_reachability.add_edge 1 [Pop 'c'] 1
      |> Test_reachability.add_edge 1 [Pop 'b'] 1
      |> Test_reachability.add_edge 1 [Pop 'a'] 2
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.of_enum states) [2] 
;;

let edge_function_reachability_test =
  "edge_function_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge_function
          (fun state ->
            if state >= 50 then Enum.empty () else
              Enum.singleton @@ ([Push 'b'], state + 1))
      |> Test_reachability.add_edge 50 [Pop 'b'] 50
      |> Test_reachability.add_edge 50 [Pop 'a'] 51
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.of_enum states) [51]
;;

let nondeterminism_reachability_test =
  "nondeterminism_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Pop 'a'] 1
      |> Test_reachability.add_edge 0 [Pop 'a'] 2
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [1;2]
;;

let dynamic_pop_reachability_test =
  "dynamic_pop_reachability_test" >:: fun _ ->
    (* The following function dynamically duplicates an element on the stack. *)
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Pop_dynamic Test_dph.Double_push] 1
      |> Test_reachability.add_edge 1 [Pop 'a'; Pop 'a'] 2
      |> Test_reachability.add_edge 1 [Pop 'a'] 3
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [2]
;;

let dynamic_pop_nondeterminism_reachability_test =
  "dynamic_pop_nondeterminism_reachability_test" >:: fun _ ->
    let dyn = Pop_dynamic Test_dph.Consume_identical_1_of_2 in
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Push 'b'; Push 'c'] 1
      |> Test_reachability.add_edge 1 [dyn] 2
      |> Test_reachability.add_edge 2 [Pop 'a'] 3
      |> Test_reachability.add_edge 0 [Push 'x'; Push 'x'] 4
      |> Test_reachability.add_edge 4 [dyn] 5
      |> Test_reachability.add_edge 5 [Pop 'a'] 6
      |> Test_reachability.add_edge 0 [Push 'y'; Push 'y'] 7
      |> Test_reachability.add_edge 7 [dyn; Pop 'a'] 8
      |> Test_reachability.add_start_state 0 'a'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [6;8]
;;

let tests = "Test_reachability" >:::
  [ immediate_reachability_test
  ; immediate_non_reachable_test
  ; two_step_reachability_test
  ; cycle_reachability_test
  ; edge_function_reachability_test
  ; nondeterminism_reachability_test
  ; dynamic_pop_reachability_test
  ; dynamic_pop_nondeterminism_reachability_test
  ]
;;
