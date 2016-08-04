{
  open Swan_generated_parser;;

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
let string_contents = [^'"']*

let ident_start = ['a'-'z']
let ident_cont = alpha | digit | '_'

let variant_start = ['A'-'Z']

rule token = parse
  | eof                              { EOF }
  | comment                          { incr_lineno lexbuf; token lexbuf }
  | whitespace                       { token lexbuf }
  | newline                          { incr_lineno lexbuf; token lexbuf }
  | "{"                              { OPEN_BRACE }
  | "}"                              { CLOSE_BRACE }
  | "["                              { OPEN_BRACKET }
  | "]"                              { CLOSE_BRACKET }
  | "("                              { OPEN_PAREN }
  | ")"                              { CLOSE_PAREN }
  | ";"                              { SEMICOLON }
  | ","                              { COMMA }
  | "->"                             { ARROW }
  | "?"                              { QUESTION_MARK }
  | "~"                              { TILDE }
  | ":"                              { COLON }
  | "::"                             { DOUBLE_COLON }
  | "="                              { EQUALS }
  | "."                              { DOT }
  | "!"                              { BANG }
  | "|"                              { PIPE }
  | "_"                              { UNDERSCORE }
  | "<-"                             { LEFT_ARROW }
  | "fun"                            { KEYWORD_FUN }
  | "let"                            { KEYWORD_LET }
  | "in"                             { KEYWORD_IN }
  | "ref"                            { KEYWORD_REF }
  | "int"                            { KEYWORD_INT }
  | "true"                           { KEYWORD_TRUE }
  | "false"                          { KEYWORD_FALSE }
  | "and"                            { KEYWORD_AND }
  | "or"                             { KEYWORD_OR }
  | "not"                            { KEYWORD_NOT }
  | "string"                         { KEYWORD_STRING }
  | "if"                             { KEYWORD_IF }
  | "then"                           { KEYWORD_THEN }
  | "else"                           { KEYWORD_ELSE }
  | "match"                          { KEYWORD_MATCH }
  | "with"                           { KEYWORD_WITH }
  | "end"                            { KEYWORD_END }
  | "any"                            { KEYWORD_ANY }
  | "staticfail"                     { KEYWORD_STATIC_FAIL }
  | "-"? digit+ as n                 { INT_LITERAL (int_of_string n) }
  | "\"" (string_contents as s) "\"" { STRING_LITERAL s }
  | "+"                              { BINOP_PLUS }
  | "-"                              { BINOP_MINUS }
  | "<"                              { BINOP_LESS }
  | "<="                             { BINOP_LESS_EQUAL }
  | "=="                             { BINOP_EQUAL }
  | ident_start ident_cont* as s     { IDENTIFIER s }
  | variant_start ident_cont* as s   { VARIANT s }
  | ";;"                             { DOUBLE_SEMICOLON }
