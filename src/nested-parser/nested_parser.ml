(**
  A front-end for the LittleBang parser library.
*)

open Batteries;;

open Nested_ast;;
open Nested_generated_lexer;;
open Nested_generated_parser;;

let parse_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    Nested_generated_parser.delim_expr Nested_generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_program (input : IO.input) =
  let buf = Lexing.from_input input in
  Nested_generated_parser.prog Nested_generated_lexer.token buf
;;