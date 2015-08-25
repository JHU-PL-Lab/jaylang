(**
  A front-end for the parser library.
*)

open Batteries;;

open Lexing;;

open Ast;;
open Generated_lexer;;
open Generated_parser;;

let parse_expressions (input : IO.input) =
  let buf = Lexing.from_input input in
  let read_expr () =
    Generated_parser.delim_expr Generated_lexer.token buf
  in
  LazyList.from_while read_expr;;

let parse_program (input : IO.input) =
  let buf = Lexing.from_input input in
  Generated_parser.prog Generated_lexer.token buf
;;