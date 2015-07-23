{
  open Nested_generated_parser;;
}

let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z']
let whitespace = [' ' '\t' '\n']
let comment = '#' [^'\n']* '\n'

let ident_start = alpha
let ident_cont = alpha | digit

rule token = parse
  | eof                              { EOF }
  | comment                          { token lexbuf }
  | whitespace                       { token lexbuf }
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
