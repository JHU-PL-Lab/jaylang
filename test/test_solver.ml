(** This module contains tests for the solver interface in the symbolic
    interpreter. *)

open Batteries;;
open OUnit2;;

module F = Formula_test_utils;;

let _tests_acc = ref [];;
let _add_test name testfn = _tests_acc := (name >:: testfn) :: !_tests_acc;;

_add_test "simple solvable" @@ fun _ ->
F.assert_solvable
  [ F.alias F.x F.y;
    F.set_int F.x 5;
  ]
;;

_add_test "simple unsolvable" @@ fun _ ->
F.assert_unsolvable
  [ F.alias F.x F.y;
    F.set_int F.x 5;
    F.set_int F.y 6;
  ]
;;

let tests = "solver tests" >::: List.rev !_tests_acc;;
