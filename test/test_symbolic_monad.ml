(** This module contains tests for the solver interface in the symbolic
    interpreter. *)

open Batteries;;
open Jhupllib;;
open OUnit2;;

open Odefa_symbolic_interpreter;;

open Symbolic_monad;;

module F = Formula_test_utils;;

(* **** Scaffolding ***** *)

let _tests_acc = ref [];;
let _add_test name testfn = _tests_acc := (name >:: testfn) :: !_tests_acc;;

(* **** Utils **** *)

let complete (x : 'a m) : ('a * Formulae.t) Enum.t =
  let rec loop (xs : 'a evaluation Enum.t) : ('a * Formulae.t) Enum.t =
    xs
    |> Enum.map
      (fun x ->
         match get_result x with
         | None -> loop @@ step x
         | Some v -> Enum.singleton (v, get_formulae x)
      )
    |> Enum.concat
  in
  loop @@ Enum.singleton @@ start x
;;

let test_complete_values
    (printer : 'a Pp_utils.pretty_printer)
    (equal : 'a -> 'a -> bool)
    (computation : 'a m)
    (expected : 'a list) : unit =
  let results = complete computation in
  let actual = List.of_enum @@ Enum.map fst results in
  assert_equal
    ~printer:(Pp_utils.pp_to_string @@ Pp_utils.pp_list printer)
    ~cmp:(List.eq equal)
    expected
    actual
;;

(* **** Tests **** *)

_add_test "pure 4" @@ fun _ ->
let computation =
  return 4
in
test_complete_values Format.pp_print_int (=) computation [4]
;;

(* **** Packaging up tests for main test module ***** *)

let tests = "symbolic monad tests" >::: List.rev !_tests_acc;;
