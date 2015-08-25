{
  open Nested_generated_parser;;

  (* ocamllex is a slave to tradition, so we have to handle line breaks
     ourselves. *)
  open Lexing
  let incr_lineno lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t']
let newline = '\n'
let comment = '#' [^'\n']* '\n'

let ident_start = alpha
let ident_cont = alpha | digit

rule token = parse
  | eof                              { EOF }
  | comment                          { incr_lineno lexbuf; token lexbuf }
  | whitespace                       { token lexbuf }
  | newline                          { incr_lineno lexbuf; token lexbuf }
  | "{"                              { OPEN_BRACE }
  | "}"                              { CLOSE_BRACE }
  | "("                              { OPEN_PAREN }
  | ")"                              { CLOSE_PAREN }
  | ","                              { COMMA }
  | "->"                             { ARROW }
  | "?"                              { QUESTION_MARK }
  | "~"                              { TILDE }
  | ":"                              { COLON }
  | "="                              { EQUALS }
  | "fun"                            { KEYWORD_FUN }
  | "let"                            { KEYWORD_LET }
  | "in"                             { KEYWORD_IN }
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | ";;"                             { DOUBLE_SEMICOLON }
