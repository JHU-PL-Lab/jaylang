(** A front-end for the parser library. *)

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

let parse_program (input : in_channel) =
  let buf = Lexing.from_channel input in
  handle_parse_error buf @@ fun () -> Parser.prog Lexer.token buf

let parse_program_str input =
  let buf = Lexing.from_string input in
  handle_parse_error buf @@ fun () -> Parser.prog Lexer.token buf

let parse_expressions_str input =
  let buf = Lexing.from_string input in
  let read_expr () =
    (handle_parse_error buf @@ fun () -> Parser.delim_expr Lexer.token buf)
    |> Option.map (fun e -> (e, ()))
  in
  Seq.unfold read_expr ()

let parse_string = parse_program_str
let parse_expressions_str = parse_expressions_str
