(** This module contains tests for the solver interface in the symbolic
    interpreter. *)

open Batteries;;
open OUnit2;;

open Odefa_symbolic_interpreter;;

open Interpreter_types;;
open Sat_types;;

let _tests_acc = ref [];;
let _add_test name testfn = _tests_acc := (name >:: testfn) :: !_tests_acc;;

let _empty_ctx = Relative_stack.empty;;
let _empty_ctx_symbol x = Symbol(Ident(x),_empty_ctx);;
let _x = _empty_ctx_symbol "x";;
let _y = _empty_ctx_symbol "y";;
let _z = _empty_ctx_symbol "z";;
let _alias s1 s2 = Formula(s1, Formula_expression_alias s2);;
let _set_int s n = Formula(s, Formula_expression_value(Value_int n));;

let _assert_solvable formulae =
  if not @@ Solve.solve @@ Formula_set.of_list formulae then
    assert_failure @@
    let msg = String.join "\n" @@ List.map show_formula formulae in
    let indented = String.nreplace ~str:msg ~sub:"\n" ~by:"\n  " in
    Printf.sprintf "Should be able to solve formula set:\n%s" indented
;;

let _assert_unsolvable formulae =
  if Solve.solve @@ Formula_set.of_list formulae then
    assert_failure @@
    let msg = String.join "\n" @@ List.map show_formula formulae in
    let indented = String.nreplace ~str:msg ~sub:"\n" ~by:"\n  " in
    Printf.sprintf "Should NOT be able to solve formula set:\n%s" indented
;;

_add_test "simple solvable" @@ fun _ ->
_assert_solvable
  [ _alias _x _y;
    _set_int _x 5;
  ]
;;

_add_test "simple unsolvable" @@ fun _ ->
_assert_unsolvable
  [ _alias _x _y;
    _set_int _x 5;
    _set_int _y 6;
  ]
;;

let tests = "solver tests" >::: List.rev !_tests_acc;;
