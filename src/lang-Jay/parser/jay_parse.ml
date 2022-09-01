module OCaml_lexing = Lexing
open Batteries
open Lexing

exception Parse_error of exn * int * int * string

let handle_parse_error buf f =
  try f ()
  with exn ->
    let curr = buf.lex_curr_p in
    let line = curr.pos_lnum in
    let column = curr.pos_cnum - curr.pos_bol in
    let tok = lexeme buf in
    raise @@ Parse_error (exn, line, column, tok)

let parse_program (input : IO.input) =
  let buf = Lexing.from_channel input in
  handle_parse_error buf (fun () -> Jay_parser.prog Jay_lexer.token buf)

let parse_program_raw (input : in_channel) =
  let buf = OCaml_lexing.from_channel input in
  handle_parse_error buf @@ fun () -> Jay_parser.prog Jay_lexer.token buf

let parse_expression_string (expr_str : string) =
  let buf = Lexing.from_string expr_str in
  let read_expr () =
    handle_parse_error buf (fun () -> Jay_parser.delim_expr Jay_lexer.token buf)
  in
  LazyList.to_list @@ LazyList.from_while read_expr

let parse_single_expr_string (expr_str : string) =
  let expr_lst = parse_expression_string expr_str in
  match expr_lst with
  | [ expr ] -> expr
  | [] -> raise @@ Invalid_argument "string is issing expression"
  | _ -> raise @@ Invalid_argument "string has more than one expression"
