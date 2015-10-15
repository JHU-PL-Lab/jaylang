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
  type state = Test_spec.state;;
  let compare_state = Test_state_ord.compare;;
  type targeted_dynamic_pop_action =
    | Double_push
    | Consume_identical_1_of_2
    | Consume_identical_2_of_2 of stack_element
    [@@deriving ord]
  ;;
  type untargeted_dynamic_pop_action =
    | Target_condition_on_element_is_A of state * state
    [@@deriving ord]
  ;;
  type stack_action =
    ( stack_element
    , targeted_dynamic_pop_action
    ) pds_stack_action
  ;;
  let pp_targeted_dynamic_pop_action = function
    | Double_push -> "Double_push"
    | Consume_identical_1_of_2 -> "Consume_identical_1_of_2"
    | Consume_identical_2_of_2(k) ->
      Printf.sprintf "Consume_identical_2_of_2(%s)"
        (Test_spec.pp_stack_element k)
  ;;
  let pp_untargeted_dynamic_pop_action = function
    | Target_condition_on_element_is_A(s1,s2) ->
      Printf.sprintf "Target_condition_on_element_is_A_pushing(%s,%s)"
        (Test_spec.pp_state s1) (Test_spec.pp_state s2)
  ;;
  let perform_targeted_dynamic_pop element action =
    match action with
    | Double_push -> Enum.singleton [Push(element);Push(element)]
    | Consume_identical_1_of_2 -> Enum.singleton
        [Pop_dynamic_targeted(Consume_identical_2_of_2 element)]
    | Consume_identical_2_of_2 element' ->
        if Test_stack_element_ord.compare element element' == 0
        then Enum.singleton []
        else Enum.empty ()
  ;;
  let perform_untargeted_dynamic_pop element action =
    match action with
    | Target_condition_on_element_is_A(s1,s2) ->
      if element == 'a'
      then Enum.singleton ([],s1)
      else Enum.singleton ([],s2)
  ;;
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

let targeted_dynamic_pop_reachability_test =
  "targeted_dynamic_pop_reachability_test" >:: fun _ ->
    (* The following function dynamically duplicates an element on the stack. *)
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0
          [Pop_dynamic_targeted Test_dph.Double_push] 1
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

let targeted_dynamic_pop_nondeterminism_reachability_test =
  "targeted_dynamic_pop_nondeterminism_reachability_test" >:: fun _ ->
    let dyn = Pop_dynamic_targeted Test_dph.Consume_identical_1_of_2 in
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

let untargeted_dynamic_pop_reachability_test =
  "untargeted_dynamic_pop_reachability_test" >:: fun _ ->
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Push 'a'] 1
      |> Test_reachability.add_untargeted_dynamic_pop_action
          1 (Test_dph.Target_condition_on_element_is_A(2,3))
      |> Test_reachability.add_edge 2 [Pop 'q'] 8
      |> Test_reachability.add_edge 3 [Pop 'q'] 9
      |> Test_reachability.add_edge 0 [Push 'b'] 11
      |> Test_reachability.add_untargeted_dynamic_pop_action
          11 (Test_dph.Target_condition_on_element_is_A(12,13))
      |> Test_reachability.add_edge 12 [Pop 'q'] 18
      |> Test_reachability.add_edge 13 [Pop 'q'] 19
      |> Test_reachability.add_edge 0 [Push 'a'] 21
      |> Test_reachability.add_edge 0 [Push 'b'] 21
      |> Test_reachability.add_untargeted_dynamic_pop_action
          21 (Test_dph.Target_condition_on_element_is_A(22,23))
      |> Test_reachability.add_edge 22 [Pop 'q'] 28
      |> Test_reachability.add_edge 23 [Pop 'q'] 29
      |> Test_reachability.add_start_state 0 'q'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = List.sort compare @@ List.of_enum @@
                    Test_reachability.get_reachable_states 0 'q' analysis in
    lazy_logger `trace
      (fun () -> "states: " ^ String_utils.pretty_list string_of_int states);
    assert_equal states [8;19;28;29]
;;

let untargeted_dynamic_pop_function_reachability_test =
  "untargeted_dynamic_pop_function_reachability_test" >:: fun _ ->
    (* We're building this:
      1. (S) -- Push 'q' --> (0)
      2. (0) -- Push 'a', Push 'b', Push 'b' --> (1)
      3. (0) -- Push 'a', Push 'a', Push 'a' --> (1)
      4. (0) -- Push 'b', Push 'b' --> (1)
      5. (0) -- Nop --> (1)
      6. (n) -- Pop 'a' --> (n+1)  for 1 <= n <= 7
      7. (n) -- Pop not 'a' --> (n+2) for 1 <= n <= 7
      8. (n) -- Pop 'q' --> (n+20) for 1 <= n <= 7
      
      Given the above, we expect the following successful paths:
      
      (S) ~1~> (0) ~2~> (1) ~7~> (3) ~7~> (5) ~6~> (6) ~7~> (8)
      (S) ~1~> (0) ~2~> (1) ~7~> (3) ~7~> (5) ~6~> (6) ~8~> (26)
      (S) ~1~> (0) ~3~> (1) ~6~> (2) ~6~> (3) ~6~> (4) ~7~> (6)
      (S) ~1~> (0) ~3~> (1) ~6~> (2) ~6~> (3) ~6~> (4) ~8~> (24)
      (S) ~1~> (0) ~4~> (1) ~7~> (3) ~7~> (5) ~7~> (7)
      (S) ~1~> (0) ~4~> (1) ~7~> (3) ~7~> (5) ~8~> (25)
      (S) ~1~> (0) ~5~> (1) ~7~> (3)
      (S) ~1~> (0) ~5~> (1) ~8~> (21)
    *)
    let edge_function state =
      if state >= 1 && state < 8
      then Enum.singleton ([Pop 'q'], state + 20)
      else Enum.empty ()
    in
    let untargeted_dynamic_pop_action_fn state =
      if state >= 1 && state < 8
      then Enum.singleton
            (Test_dph.Target_condition_on_element_is_A(state+1,state+2))
      else Enum.empty ()
    in 
    let analysis =
      Test_reachability.empty
      |> Test_reachability.add_edge 0 [Push 'a'; Push 'b'; Push 'b'] 1
      |> Test_reachability.add_edge 0 [Push 'a'; Push 'a'; Push 'a'] 1
      |> Test_reachability.add_edge 0 [Push 'b'; Push 'b'] 1
      |> Test_reachability.add_edge 0 [] 1
      |> Test_reachability.add_edge_function edge_function
      |> Test_reachability.add_untargeted_dynamic_pop_action_function
          untargeted_dynamic_pop_action_fn
      |> Test_reachability.add_start_state 0 'q'
    in
    lazy_logger `trace
      (fun () -> "analysis:\n" ^
        String_utils.indent 2 (Test_reachability.pp_analysis analysis));
    let states = List.sort compare @@ List.of_enum @@
                    Test_reachability.get_reachable_states 0 'q' analysis in
    lazy_logger `trace
      (fun () -> "states: " ^ String_utils.pretty_list string_of_int states);
    assert_equal states [3;6;7;8;21;24;25;26]
;;

let tests = "Test_reachability" >:::
  [ immediate_reachability_test
  ; immediate_non_reachable_test
  ; two_step_reachability_test
  ; cycle_reachability_test
  ; edge_function_reachability_test
  ; nondeterminism_reachability_test
  ; targeted_dynamic_pop_reachability_test
  ; targeted_dynamic_pop_nondeterminism_reachability_test
  ; untargeted_dynamic_pop_reachability_test
  ; untargeted_dynamic_pop_function_reachability_test
  ]
;;
