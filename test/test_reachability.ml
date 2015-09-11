(**
  This test module performs a series of operations to test the PDA reachability
  functionality in the Odefa analysis library.
*)

open Batteries;;
open OUnit2;;

let lazy_logger = Logger_utils.make_lazy_logger "Test_reachability";;

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

module Test_reachability = Pds_reachability.Make(Test_spec);;

open Test_reachability;;

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
    let states = Test_reachability.get_reachable_states 0 'a' analysis in
    assert_equal (List.sort compare @@ List.of_enum states) [1;2]    

let tests = "Test_reachability" >:::
  [ immediate_reachability_test
  ; immediate_non_reachable_test
  ; two_step_reachability_test
  ; cycle_reachability_test
  ; edge_function_reachability_test
  ; nondeterminism_reachability_test
  ]
;;
