(**
   This module contains a number of utilty functions used to create formulae.
*)

open Batteries;;
open OUnit2;;

open Odefa_ast;;
open Odefa_symbolic_interpreter;;

open Ast;;
open Interpreter_types;;
open Sat_types;;

let empty_stack = Relative_stack.empty;;
let empty_stack_symbol x = Symbol(Ident(x), empty_stack);;
let symbol_with_stack x s =
  Symbol(Ident(x),
         s
         |> List.map
           (fun name ->
              Clause(Var(Ident(name),None),Value_body(Value_int 0)))
         |> List.fold_left
           (fun a e -> Option.get @@ Relative_stack.push a e)
           Relative_stack.empty
        )
;;

let x = empty_stack_symbol "x";;
let y = empty_stack_symbol "y";;
let z = empty_stack_symbol "z";;
let xa = symbol_with_stack "x" ["a"];;

let alias s1 s2 = Formula(s1, Formula_expression_alias s2);;
let set_int s n = Formula(s, Formula_expression_value(Value_int n));;

let assert_solvable formula_list =
  let formulae = Formulae.of_enum @@ List.enum formula_list in
  if not @@ Solve.solve formulae then
    assert_failure @@
    let msg =
      String.join "\n" @@ List.map show_formula @@
      List.of_enum @@ Formulae.enum formulae
    in
    let indented = String.nreplace ~str:msg ~sub:"\n" ~by:"\n  " in
    Printf.sprintf "Should be able to solve formula set:\n%s" indented
;;

let assert_unsolvable formula_list =
  let formulae = Formulae.of_enum @@ List.enum formula_list in
  if Solve.solve formulae then
    assert_failure @@
    let msg =
      String.join "\n" @@ List.map show_formula @@
      List.of_enum @@ Formulae.enum formulae
    in
    let indented = String.nreplace ~str:msg ~sub:"\n" ~by:"\n  " in
    Printf.sprintf "Should NOT be able to solve formula set:\n%s" indented
;;
