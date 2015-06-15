(**
  A front-end for the parser library.
*)

open Batteries;;

open Toy_ast;;
open Toy_generated_lexer;;
open Toy_generated_parser;;

let parse_toy_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    Toy_generated_parser.delim_expr Toy_generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_toy_program (input : IO.input) =
  let buf = Lexing.from_input input in
  Toy_generated_parser.prog Toy_generated_lexer.token buf
;;
